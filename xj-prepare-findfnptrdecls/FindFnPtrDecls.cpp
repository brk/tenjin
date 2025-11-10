#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/Execution.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

namespace {
StringSet ModifiedFnNames;
bool is_modified_fn_name(const std::string &s) {
  return ModifiedFnNames.contains(s);
}

class FindFnPtrDeclsCallback : public MatchFinder::MatchCallback {
public:
  FindFnPtrDeclsCallback(ExecutionContext &Context)
      : Context(Context), SM(nullptr) {}

  void run(const MatchFinder::MatchResult &Result) override {
    if (!SM) {
      SM = Result.SourceManager;
    }

    auto *ILE = Result.Nodes.getNodeAs<InitListExpr>("init_list_expr");
    if (ILE && ILE->getBeginLoc().isValid()) {
      handle_init_list_expr(ILE, Result);
      return;
    }

    auto *D = Result.Nodes.getNodeAs<Stmt>("assign_to_decl");
    if (D && D->getBeginLoc().isValid()) {
      handle_assign_to_decl(D, Result);
      return;
    }
  }

  void handle_assign_to_decl(const Stmt *D,
                             const MatchFinder::MatchResult &Result) {
    auto *rhs = Result.Nodes.getNodeAs<DeclRefExpr>("rhs");
    if (rhs && rhs->getBeginLoc().isValid()) {
      std::string rhs_name = rhs->getNameInfo().getName().getAsString();
      if (is_modified_fn_name(rhs_name)) {
        auto *lhs_dd = Result.Nodes.getNodeAs<DeclaratorDecl>("lhs_decl");
        if (lhs_dd) {
          QualType QT = lhs_dd->getType();

          // Strip pointer to get function type
          if (const PointerType *PT = QT->getAs<PointerType>()) {
            QualType PointeeType = PT->getPointeeType();

            if (const FunctionProtoType *FPT =
                    PointeeType->getAs<FunctionProtoType>()) {
              // Get the TypeSourceInfo for better location tracking
              TypeSourceInfo *TSI = lhs_dd->getTypeSourceInfo();
              if (TSI) {
                TypeLoc TL = TSI->getTypeLoc();

                // Navigate through pointer type loc
                if (PointerTypeLoc PTL = TL.getAs<PointerTypeLoc>()) {
                  TypeLoc PointeeTL = PTL.getPointeeLoc();
                  // llvm::outs() << "PointeeTL = " <<
                  // PointeeTL.getType().getAsString() << "\n";
                  if (try_find_fn_ptr_lparen_loc(PointeeTL)) {
                    // pass
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  void handle_init_list_expr(const InitListExpr *ILE,
                             const MatchFinder::MatchResult &Result) {
    SmallVector<int> field_nums;
    for (unsigned i = 0; i < ILE->getNumInits(); ++i) {
      const Expr *e = ILE->getInit(i)->IgnoreParenCasts();
      if (const DeclRefExpr *dre = dyn_cast<DeclRefExpr>(e)) {
        std::string name = dre->getDecl()->getNameAsString();
        if (is_modified_fn_name(name)) {
          // Context.reportResult(Change.getKey(), std::string("initlist -> ") +
          // name);
          field_nums.push_back(i);
        }
      }
    }

    int curr_num = 0;
    QualType QT = ILE->getType();
    if (const RecordType *RT = QT->getAs<RecordType>()) {
      RecordDecl *RD = RT->getDecl();
      auto FieldIt = RD->field_begin();
      for (unsigned i = 0; i < field_nums.size(); ++i) {
        while (curr_num < field_nums[i]) {
          ++curr_num;
          ++FieldIt;
        }
        FieldDecl *TargetField = *FieldIt;
        auto TSI = TargetField->getTypeSourceInfo();
        if (TSI) {
          if (try_find_fn_ptr_lparen_loc(TSI->getTypeLoc())) {
            // pass
          } else {
            llvm::errs() << "unable to find fn ptr lparen loc for InitListExpr"
                         << "\n";
          }
        } else {
          llvm::errs() << "unable to find fn ptr TSI for InitListExpr" << "\n";
        }
      }
    }
  }

  bool try_find_fn_ptr_lparen_loc(TypeLoc TL) {
    while (1) {
      // FunctionProtoTypeLoc is the one that has parameter parens
      if (auto FPTL = TL.getAs<FunctionProtoTypeLoc>()) {
        FnPtrTypeOpenParens[FPTL.getLParenLoc()] = FPTL.getRParenLoc();
        return true;
      }
      if (auto FNPL = TL.getAs<FunctionNoProtoTypeLoc>()) {
        FnPtrTypeOpenParens[FNPL.getLParenLoc()] = FNPL.getRParenLoc();
        return true;
      }

      // walk "inward" one TypeLoc
      TL = TL.getNextTypeLoc();
      if (TL.isNull())
        break; // defensive
    }
    return false;
  }

  void onStartOfTranslationUnit() override {
    SM = nullptr;
    FnPtrTypeOpenParens.clear();
  }

  void onEndOfTranslationUnit() override {
    if (!SM && !FnPtrTypeOpenParens.empty()) {
      Context.reportResult(
          "END",
          "End of TU -- no SourceManager but had identified open parens");
      return;
    }

    for (auto &[Loc, RLoc] : FnPtrTypeOpenParens) {
        auto F = SM->getFilename(Loc);
        byFile[F].push_back(
            std::make_pair(SM->getFileOffset(Loc), SM->getFileOffset(RLoc)));
    }
  }

 // Prints a JSON dict of the form
 // ```
 //     { "<FILEPATH_1>":[ [o1,c1], [o2,c2], ...],
 //       "<FILEPATH_2>":[...], ... }
 // ```
 void emitJSONDictForModifiedFnPtrTypeLocs() {
    if (!byFile.empty()) {
      llvm::outs() << "{" << "\n";
      bool firstfile = true;
      for (auto &[F, Offsets] : byFile) {
        if (!firstfile) {
          llvm::outs() << ",\n";
        } else {
          firstfile = false;
        }

        llvm::outs() << "\"" << F << "\""
                     << ":" << "\n"
                     << "[";

        bool first = true;
        for (auto Off : Offsets) {
          if (!first) {
            llvm::outs() << ", ";
          } else {
            first = false;
          }
          llvm::outs() << "[" << Off.first << ", " << Off.second << "]";
        }
        llvm::outs() << "]";
      }
      llvm::outs() << "}" << "\n";
    }
    // Context.reportResult("END", "End of TU.");
  }

private:
  ExecutionContext &Context;
  SourceManager *SM;

  DenseMap<SourceLocation, SourceLocation>
      FnPtrTypeOpenParens; // maps left paren to right paren
  StringMap<SmallVector<std::pair<int, int>>> byFile; // file -> list[pair[offset]]
};
} // end anonymous namespace

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::OptionCategory
    FindFnPtrDeclsCategory("xj-find-fn-ptr-decls options");

static cl::opt<std::string> ModifiedFunctionNamesFile(
    "modified_fns_file",
    cl::desc("File containing names of modified functions"),
    cl::cat(FindFnPtrDeclsCategory));

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  auto Executor = clang::tooling::createExecutorFromCommandLineArgs(
      argc, argv, FindFnPtrDeclsCategory);

  if (!Executor) {
    llvm::errs() << llvm::toString(Executor.takeError()) << "\n";
    return 1;
  }

  if (ModifiedFunctionNamesFile.empty()) {
    llvm::errs() << "Must provide path for --modified_fns_file" << "\n";
    return 1;
  } else {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> Buffer =
        llvm::MemoryBuffer::getFile(ModifiedFunctionNamesFile);

    if (auto Err = Buffer.getError()) {
      llvm::errs() << "Unable to open " << ModifiedFunctionNamesFile << ": "
                   << Err.message() << "\n";
      return 1;
    }

    llvm::StringRef FileContent = Buffer.get()->getBuffer();
    while (!FileContent.empty()) {
      llvm::StringRef Name;
      std::tie(Name, FileContent) = FileContent.split("\n");
      ModifiedFnNames.insert(Name);
    }
  }

  ast_matchers::MatchFinder Finder;
  FindFnPtrDeclsCallback Callback(*Executor->get()->getExecutionContext());

  Finder.addMatcher(
      binaryOperator(
          hasOperatorName("="),
          hasLHS(declRefExpr(hasDeclaration(declaratorDecl().bind("lhs_decl")))
                     .bind("lhs")),
          hasRHS(expr(ignoringImpCasts(declRefExpr().bind("rhs")))))
          .bind("assign_to_decl"),
      &Callback);

  Finder.addMatcher(initListExpr(has(expr(ignoringImpCasts(declRefExpr()))))
                        .bind("init_list_expr"),
                    &Callback);

  auto Err = Executor->get()->execute(newFrontendActionFactory(&Finder));
  if (Err) {
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
  }
  Executor->get()->getToolResults()->forEachResult(
      [](llvm::StringRef key, llvm::StringRef value) {
        llvm::errs() << "----" << key.str() << "\n" << value.str() << "\n";
      });

  Callback.emitJSONDictForModifiedFnPtrTypeLocs();
}
