#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
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

/*
Some examples of Clang's source locations:
(pointers (^) are "after" the location indicated)

col
0        1         2         3         4         5         6         7         8
1234567890123456789012345678901234567890

int bvar[2], *cvar, *dvar[2];
^   ^      ^   
|   |      +- typeloc end   (col 11), also getEndLoc() of getSourceRange()
|   +-------- getLocation() (col 5) for `bvar`
+------------ typeloc begin       , also getBeginLoc() of getSourceRange()


int bvar[2], *cvar, *dvar[2];
^            ^^
|            |+-- getLocation() (col 5) for `cvar`
|            | &  getEndLoc() of getSourceRange()  (col )
|            |
|            +- typeloc end   (col 14)
+-------------- typeloc begin, also getBeginLoc() of getSourceRange()


                               +-- TL[0]<ArrayTypeLoc> getBracketsRange().getStart()
                               v
const int bvar[2], *cvar, *dvar[2];
^     ^                   ^      ^
|   ++ TL[0] begin        |      +- TL[0] end
|    & TL[2] end          +-- TL[1] end
|
+    

==============================================================

#define INTVAR(q) int q, yyyy
                  ^      ^
                  |      +--- getSpellingLoc(getLocation()) for `yyyy`
                  +---------- typeloc end for `yyyy` 
signed INTVAR(xxxx), *zzzz;
^      ^
|      +---- getLocation() for `yyyy`
+---- typeloc begin for `yyyy`, also getBeginLoc() of getSourceRange()

* The get_tokens() function in the Python API returns
    [signed,INTVAR,(,x,)]
  for the `yyyy` decl!


VarDecl::getPreviousDecl() does not return adjacent decls of different vars,
it only returns previous decls of the same var.

Lexer::getSourceText() is ""                           for xxxx
Lexer::getSourceText() is "signed INTVAR(xxxx)"        for yyyy
Lexer::getSourceText() is "signed INTVAR(xxxx), *zzzz" for zzzz

==============================================================

                 +--- typeloc begin & typeloc end
                 |
                 v
#define MULTI(x) int x ## 1; int e2 , x ## 3;
MULTI(e)                         
^                                
|                                
+----- getLocation() for `e1`

? <--- SM->getSpellingLoc(D->getLocation()) [<scratch space>:2:1]

* The get_tokens() function in the Python API returns
    [int,x,##,...,##,3,;,MULTI,(,e,)]
  for the `e1` decl!




                 +--- typeloc begin & typeloc end
                 |
                 v
#define MULTI(x) int x ## 1; int e2 , x ## 3;
MULTI(e)                         ^
^                                +--- getLocation() for `e2` (offset 16)
|                                +--- getSpellingLoc(getLocation()) (offset 243)
+----- getFileLoc(getLocation()) for `e2`
     = getExpansionLoc(getLocation()) for `e2`

Lexer::getSourceText() returns empty string for all 3 decls.
*/


namespace {

const char* LINEPREFIX = "        ";

class PrintDeclLocsCallback : public MatchFinder::MatchCallback {
public:
  PrintDeclLocsCallback(ExecutionContext &Context)
      : /*Context(Context),*/ SM(nullptr), Ctx(nullptr) {}

  void run(const MatchFinder::MatchResult &Result) override {
    if (!SM) {
      SM = Result.SourceManager;
      Ctx = Result.Context;
    }

    if (auto *FD = Result.Nodes.getNodeAs<FieldDecl>("fieldDecl")) {
      processDeclLocations("FieldDecl", FD);
    } else if (auto *TD = Result.Nodes.getNodeAs<TypedefDecl>("typedefDecl")) {
      processDeclLocations("TypedefDecl", TD);
    } else if (auto *VD = Result.Nodes.getNodeAs<VarDecl>("varDecl")) {
      processDeclLocations("VarDecl", VD);
    }
  }

  void processDeclLocations(const char* declType, const NamedDecl* D) {
    if (!D || !D->getLocation().isValid()) {
      return;
    }

    SrcRangeExpStartLocToDeclMap[SM->getExpansionLoc(D->getSourceRange().getBegin())].push_back(D);
  }

#if 0  
  void printLocation(SourceLocation BeginLoc, SourceLocation EndLoc,
                     SourceLocation Loc, const char* locName, bool isSingleLine) {

    bool withinExtent = SM->getFileOffset(Loc) >= SM->getFileOffset(BeginLoc) &&
                        SM->getFileOffset(Loc) <= SM->getFileOffset(EndLoc);
    if (isSingleLine && withinExtent) {
      
        auto col = SM->getSpellingColumnNumber(Loc);
        std::string padded(col, ' ');
        llvm::outs() << LINEPREFIX << padded << "^\n";
        llvm::outs() << LINEPREFIX << padded << "+-- getSpellingColumnNumber(" << locName << ")"
                      << " = " << col << "\n";
    } else {
      llvm::outs() << Loc.printToString(*SM)
                   << " (Line " << SM->getSpellingLineNumber(Loc)
                   << ", Col " << SM->getSpellingColumnNumber(Loc)
                   << ", offset " << SM->getFileOffset(Loc) << ")";
      llvm::outs() << " -- " << locName;
    }
    llvm::outs() << "\n";
    llvm::outs() << "isMacroArgExpansion: "
                 << (SM->isMacroArgExpansion(Loc) ? "true" : "false") << "\n";
    llvm::outs() << "isMacroBodyExpansion: "
                 << (SM->isMacroBodyExpansion(Loc) ? "true" : "false") << "\n";
                 bool invalid = false;
    llvm::outs() << "getExpansionColumnNumber: "
                 << SM->getExpansionColumnNumber(Loc, &invalid);
    llvm::outs() << (invalid ? "(invalid)" : "") << "\n";
    llvm::outs() << "getFileOffset: "
                 << SM->getFileOffset(Loc) << "\n";
    // invalid = false;
    // const char* cd = SM->getCharacterData(Loc, &invalid);
    // int len = strlen(cd);
    // if (cd) {
    //   llvm::outs() << "getCharacterData had strlen " << len << "\n";
    //   llvm::outs() << "  ==> \"" << StringRef(cd, std::min(len, 60)) << "\"...\n";
    // } else {
    //   llvm::outs() << "getCharacterData: nullptr\n";
    // }
    // llvm::outs() << (invalid ? "(invalid)" : "") << "\n";
  }

  void printPresuLoc(SourceLocation BeginLoc, SourceLocation EndLoc,
                     PresumedLoc Loc, const char* locName, bool isSingleLine) {
    if (!Loc.isValid()) {
      llvm::outs() << "(invalid PresumedLocation) -- " << locName << "\n";
      return;
    }
    llvm::outs() << Loc.getFilename() << ":"
                 << Loc.getLine() << ":"
                 << Loc.getColumn();
    if (Loc.getIncludeLoc().isValid()) {
      llvm::outs() << " (included at "
                   << Loc.getIncludeLoc().printToString(*SM)
                   << ")";
    } else {
      llvm::outs() << " (no #include loc)";
    }
    llvm::outs() << " -- " << locName << "\n";
  }

  template<typename DeclTy>
  void printDeclLocations(const char* declType, const DeclTy* D) {
    if (!D || !D->getLocation().isValid()) {
      return;
    }

    // Skip declarations not from the main file
    // if (!SM->isInMainFile(D->getLocation())) {
    //   return;
    // }

    llvm::outs() << "\n=== " << declType << ": " << D->getNameAsString() << " ===\n";

    llvm::outs() << "SourceRange: " << D->getSourceRange().printToString(*SM) << "\n";

    // Get the source range
    SourceRange SR = D->getSourceRange();
    SourceLocation BeginLoc = SR.getBegin();
    SourceLocation EndLoc = SR.getEnd();

    // Get line and column information
    unsigned BeginLine = SM->getSpellingLineNumber(BeginLoc);
    unsigned BeginCol = SM->getSpellingColumnNumber(BeginLoc);
    unsigned EndLine = SM->getSpellingLineNumber(EndLoc);
    unsigned EndCol = SM->getSpellingColumnNumber(EndLoc);

    // Check if the declaration is on a single line
    bool isSingleLine = (BeginLine == EndLine);

    if (isSingleLine) {
      // Get the entire line of source code
      bool Invalid = false;
      StringRef LineText = SM->getBufferData(SM->getFileID(BeginLoc), &Invalid);
      if (!Invalid) {
        // Find the line start and end
        unsigned FileOffset = SM->getFileOffset(BeginLoc);
        unsigned LineStartOffset = FileOffset - (BeginCol - 1);
        
        // Extract the line
        size_t LineStart = LineStartOffset;
        size_t LineEnd = LineText.find('\n', LineStart);
        if (LineEnd == StringRef::npos) {
          LineEnd = LineText.size();
        }
        StringRef Line = LineText.slice(LineStart, LineEnd);
        
        llvm::outs() << "Line " << BeginLine << ": " << Line << "\n";
        
        // Create pointer line showing the extent
        std::string PointerLine(Line.size(), ' ');
        unsigned ExtentStart = BeginCol - 1;
        unsigned ExtentEnd = EndCol - 1;
        for (unsigned i = ExtentStart; i <= ExtentEnd && i < PointerLine.size(); ++i) {
          PointerLine[i] = '^';
        }
        llvm::outs() << LINEPREFIX << PointerLine << "\n";
      } else {
        llvm::outs() << "Line " << BeginLine << ": <unable to retrieve line text>\n";
        llvm::outs() << "(treating as if not single-line)\n";
        llvm::outs() << "BeginLoc.isValid(): " << (BeginLoc.isValid() ? "true" : "false") << "\n";
        llvm::outs() << "BeginLoc.isFileID(): " << (BeginLoc.isFileID() ? "true" : "false") << "\n";
        llvm::outs() << "BeginLoc.isMacroID(): " << (BeginLoc.isMacroID() ? "true" : "false") << "\n";
        isSingleLine = false;
      }
    }
    
    if (!isSingleLine){
      llvm::outs() << "Source Range: " << SR.printToString(*SM) << "\n";
      llvm::outs() << "  Begin: Line " << BeginLine << ", Col " << BeginCol 
                   << " (offset " << SM->getFileOffset(BeginLoc) << ")\n";
      llvm::outs() << "  End: Line " << EndLine << ", Col " << EndCol 
                   << " (offset " << SM->getFileOffset(EndLoc) << ")\n";
    }

    // Print various location attributes
    printLocation(BeginLoc, EndLoc, D->getLocation(),
                                   "D->getLocation()", isSingleLine);
    printLocation(BeginLoc, EndLoc, SM->getFileLoc(D->getLocation()),
                                   "SM->getFileLoc(D->getLocation())", isSingleLine);
    printLocation(BeginLoc, EndLoc, SM->getExpansionLoc(D->getLocation()),
                                   "SM->getExpansionLoc(D->getLocation())", isSingleLine);
    printLocation(BeginLoc, EndLoc, SM->getSpellingLoc(D->getLocation()),
                                   "SM->getSpellingLoc(D->getLocation())", isSingleLine);
    printLocation(BeginLoc, EndLoc, SM->getImmediateSpellingLoc(D->getLocation()),
                                   "SM->getImmediateSpellingLoc(D->getLocation())", isSingleLine);
    printPresuLoc(BeginLoc, EndLoc, SM->getPresumedLoc(D->getLocation()),
                                   "SM->getPresumedLoc(D->getLocation())", isSingleLine);
    printPresuLoc(BeginLoc, EndLoc, SM->getPresumedLoc(D->getLocation(), false),
                                   "SM->getPresumedLoc(D->getLocation(), UseLineDirectives=false)", isSingleLine);

    if (auto *VD = dyn_cast<VarDecl>(D)) {
      llvm::outs() << "VarDecl getPreviousDecl() returned... " << (VD->getPreviousDecl() ? "a decl" : "nullptr") << "\n";
    }

    // For DeclaratorDecl (VarDecl, FieldDecl), check TypeSourceInfo
    if (auto *DD = dyn_cast<DeclaratorDecl>(D)) {
      if (TypeSourceInfo *TSI = DD->getTypeSourceInfo()) {
        TypeLoc TL = TSI->getTypeLoc();

        // not in LLVM 18 i think
        //printLocation(BeginLoc, EndLoc, TL.getNonElaboratedBeginLoc(), "getNonElaboratedBeginLoc", isSingleLine);

        int next = 0;
        do {
          llvm::outs() << "---- next TypeLoc; level = " << next << " ----\n";
          printLocation(BeginLoc, EndLoc, TL.getBeginLoc(), "TypeBeginLoc", isSingleLine);
          printLocation(BeginLoc, EndLoc, TL.getEndLoc(), "TypeEndLoc", isSingleLine);

          TypeLoc fEQL = TL.findExplicitQualifierLoc();
          if (!fEQL.isNull()) {
            printLocation(BeginLoc, EndLoc, fEQL.getBeginLoc(),
                                        "findExplicitQualifierLoc BeginLoc", isSingleLine);
          }
          

          ArrayTypeLoc ATL = TL.getAs<ArrayTypeLoc>();
          if (!ATL.isNull()) {
            printLocation(BeginLoc, EndLoc, ATL.getBracketsRange().getBegin(),
                                        "ArrayTypeLoc BracketBeginLoc", isSingleLine);
          }

          PointerTypeLoc PTL = TL.getAs<PointerTypeLoc>();
          if (!PTL.isNull()) {
            printLocation(BeginLoc, EndLoc, PTL.getStarLoc(),
                                        "PointerTypeLoc StarLoc", isSingleLine);
          }

          QualifiedTypeLoc QTL = TL.getAs<QualifiedTypeLoc>();
          if (!QTL.isNull()) {
            UnqualTypeLoc UTL = QTL.getUnqualifiedLoc();
            printLocation(BeginLoc, EndLoc, UTL.getBeginLoc(),
                                        "QaulTypeLoc->UnqualTypeLoc BeginLoc", isSingleLine);
          }

          llvm::errs() << "type isConstQualified: " << TL.getType().isConstQualified() << "\n";


          TL = TL.getNextTypeLoc();
          next++;
        } while (!TL.isNull() && next < 10);
      }
    }

    // For TypedefDecl, check TypeSourceInfo
    if (auto *TD = dyn_cast<TypedefDecl>(D)) {
      if (TypeSourceInfo *TSI = TD->getTypeSourceInfo()) {
        TypeLoc TL = TSI->getTypeLoc();
        int next = 0;
        do {
          llvm::outs() << "---- next TypeLoc; level = " << next << " ----\n";
          printLocation(BeginLoc, EndLoc, TL.getBeginLoc(), "TypeBeginLoc", isSingleLine);
          printLocation(BeginLoc, EndLoc, TL.getEndLoc(), "TypeEndLoc", isSingleLine);
          TL = TL.getNextTypeLoc();
          next++;
        } while (!TL.isNull() && next < 10);
      }
    }

    // Print the source text
    CharSourceRange CharRange = CharSourceRange::getTokenRange(SR);
    StringRef SourceText = Lexer::getSourceText(CharRange, *SM, Ctx->getLangOpts());
    llvm::outs() << "Lexer::getSourceText (token range): ==============\n" << SourceText << "\n================\n";

    llvm::outs() << "Lexer::getSourceText (char range): ==============\n" <<
      Lexer::getSourceText(Lexer::getAsCharRange(
                              CharSourceRange::getTokenRange(SR), *SM, Ctx->getLangOpts()),
                           *SM, Ctx->getLangOpts())
      << "\n================\n";
    llvm::outs() << "CharRange isValid: " << (CharRange.isValid() ? "true" : "false") << "\n";
    
  }
#endif

  void onStartOfTranslationUnit() override {
    SM = nullptr;
    Ctx = nullptr;
  }

  void onEndOfTranslationUnit() override {
    StringMap<std::vector<SourceLocation>> failedDecls;
    StringMap<std::vector<SourceLocation>> curiousDecls;

    for (auto &[Loc, Decls] : SrcRangeExpStartLocToDeclMap) {
      if (Decls.size() < 2) {
        continue;
      }
      if (ProcessedDeclLocs.contains(Loc)) {
        continue;
      }
      // We don't wait until the end of the TU to mark it as processed,
      // because the same TU might include the same header (or .inc) file
      // in multiple places with multiple macro contexts/bindings. We still
      // want to process each occurrence in the header just once. The source
      // text we'll compute below will be of the header file before macro
      // expansion, which is what we want.
      ProcessedDeclLocs.insert(Loc);

      auto FC = SM->getFileCharacteristic(SM->getExpansionLoc(Loc));
      if (FC != SrcMgr::C_User) {
        // silently skip decls from system headers
        continue;
      }

      // llvm::outs() << "\n=== Decls at " << Loc.printToString(*SM) << "\n";
      // llvm::outs() << "          SP " << SM->getSpellingLoc(Loc).printToString(*SM) << "\n";
      // llvm::outs() << "       ImmSP " << SM->getImmediateSpellingLoc(Loc).printToString(*SM) << "\n";
      // llvm::outs() << "         EXP " << SM->getExpansionLoc(Loc).printToString(*SM) << "\n";

      // for (const NamedDecl* D : Decls) {
      //   llvm::outs() << "  - " << D->getDeclKindName() << ": " << D->getNameAsString() <<
      //                   D->getSourceRange().printToString(*SM) << "\n";
      // }
      
      bool adjacent_statements_from_macro_expansion = false;
      bool declarators_defined_within_same_macro = false;

      for (const NamedDecl* D : Decls) {
        
        for (const NamedDecl* Dother : Decls) {
          if (D == Dother) continue;

          bool sameStartExpLoc = SM->getExpansionLoc(D->getSourceRange().getBegin()) ==
                            SM->getExpansionLoc(Dother->getSourceRange().getBegin());
          bool sameStartSpellLoc = SM->getSpellingLoc(D->getSourceRange().getBegin()) ==
                              SM->getSpellingLoc(Dother->getSourceRange().getBegin());
          if (sameStartExpLoc && !sameStartSpellLoc) {
            // llvm::outs() << "  DECLS WITH SAME EXPANSION LOC BUT DIFFERENT SPELLING LOC!\n";
            // llvm::outs() << "    D: " << D->getDeclKindName() << " " << D->getNameAsString() << "\n";
            // llvm::outs() << "      SpellLoc: " << SM->getSpellingLoc(D->getSourceRange().getBegin()).printToString(*SM) << "\n";
            // llvm::outs() << "    Dother: " << Dother->getDeclKindName() << " " << Dother->getNameAsString() << "\n";
            // llvm::outs() << "      SpellLoc: " << SM->getSpellingLoc(Dother->getSourceRange().getBegin()).printToString(*SM) << "\n";
            adjacent_statements_from_macro_expansion = true;
            break;
          }

          bool sameIdentExpLoc = SM->getExpansionLoc(D->getLocation()) ==
                            SM->getExpansionLoc(Dother->getLocation());
          bool sameIdentSpellLoc = SM->getSpellingLoc(D->getLocation()) ==
                              SM->getSpellingLoc(Dother->getLocation());
          if (sameIdentExpLoc) {
            // llvm::outs() << "  DECL IDENTS WITH SAME EXPANSION LOC!\n";
            // llvm::outs() << "    D: " << D->getDeclKindName() << " " << D->getNameAsString() << "\n";
            // llvm::outs() << "      SpellLoc: " << SM->getSpellingLoc(D->getSourceRange().getBegin()).printToString(*SM) << "\n";
            // llvm::outs() << "    Dother: " << Dother->getDeclKindName() << " " << Dother->getNameAsString() << "\n";
            // llvm::outs() << "      SpellLoc: " << SM->getSpellingLoc(Dother->getSourceRange().getBegin()).printToString(*SM) << "\n";
            declarators_defined_within_same_macro = true;
            break;
          }
        }
      }

      if (adjacent_statements_from_macro_expansion) {
        failedDecls["macro_multi_stmt"].push_back(Loc);
        continue;
      }
      if (declarators_defined_within_same_macro) {
        failedDecls["macro_multi_declarator"].push_back(Loc);
        continue;
      }

      const NamedDecl* leftmostDecl = nullptr;
      const NamedDecl* rightmostDecl = nullptr;

      for (const NamedDecl* D : Decls) {
        if (!leftmostDecl || SM->isBeforeInTranslationUnit(D->getLocation(), leftmostDecl->getLocation())) {
          leftmostDecl = D;
        }
        if (!rightmostDecl || SM->isBeforeInTranslationUnit(rightmostDecl->getLocation(), D->getLocation())) {
          rightmostDecl = D;
        }
      }

      SourceLocation leftmostSrcLoc = leftmostDecl->getLocation();
      SourceLocation declEndLoc = rightmostDecl->getSourceRange().getEnd();


      // PREFIX is everything from the SRSL to the Location of the leftmost Decl,
      // minus any preceding star characters,
      // and one can find the splitting commas by walking backward from the
      // ExpansionLoc of the remaining Decls.
      SourceLocation leftmostNonStarLoc = leftmostSrcLoc;
      while (1) {
        // INVARIANT: every decl's Loc has a distinct ExpansionLoc
        std::optional< Token > opt_prev = Lexer_findPreviousToken(
            SM->getExpansionLoc(leftmostSrcLoc), *SM, Ctx->getLangOpts());
        if (!opt_prev.has_value()) { 
          //llvm::outs() << "       no previous token\n";
          break; }
        Token prev = opt_prev.value();
        if (prev.is(tok::star) || prev.is(tok::l_paren)) {
          leftmostSrcLoc = prev.getLocation();
        } else {
          leftmostNonStarLoc = leftmostSrcLoc;          
          std::optional<Token> opt_next = Lexer::findNextToken(
              prev.getLocation(), *SM, Ctx->getLangOpts());
          if (!opt_next.has_value()) { 
            //llvm::outs() << "no token after previous token!!!!\n";
            break;
          }
          leftmostSrcLoc = opt_next.value().getLocation();
          break;
        }
      }

      std::vector<SourceLocation> splittingCommaLocs;
      for (const NamedDecl* D : Decls) {
        if (D == leftmostDecl) { continue; }
        SourceLocation Loc = D->getLocation();
        // INVARIANT: every decl's Loc has a distinct ExpansionLoc
        while (1) {
          std::optional< Token > opt_prev = Lexer_findPreviousToken(SM->getExpansionLoc(Loc), *SM, Ctx->getLangOpts());
          if (!opt_prev.has_value()) { break; }
          Token prev = opt_prev.value();
          if (prev.is(tok::comma)) {
            splittingCommaLocs.push_back(prev.getLocation());
            break;
          } else {
            Loc = prev.getLocation();
            if (SM->isBeforeInTranslationUnit(Loc, leftmostSrcLoc)) {
              // did not find comma before reaching leftmostDecl
              break;
            }
          }
        }
      }

      CharSourceRange prefixCharRange = CharSourceRange::getCharRange(
          leftmostDecl->getSourceRange().getBegin(), leftmostNonStarLoc);
      StringRef prefixText = Lexer::getSourceText(
          prefixCharRange, *SM, Ctx->getLangOpts());

      bool hasPreprocessorDirectives = false;
      for (char c : prefixText) {
        // This is a false positive for comments that contain #,
        // but good enough for now.
        if (c == '#') {
          hasPreprocessorDirectives = true;
        }
      }

      if (hasPreprocessorDirectives) {
        failedDecls["directive_in_prefix"].push_back(Loc);
        continue;
      }

      if (prefixText.empty()) {
        failedDecls["empty_prefix"].push_back(Loc);
        continue;
      }

      if (prefixText[prefixText.size()-1] != ' ') {
        curiousDecls["prefix_ends_with_nonspace"].push_back(Loc);
      }

      std::vector<std::string> declaratorTexts;
      
      // llvm::outs() << "\n--- Declaration Splitter Info ---\n";
      // llvm::outs() << "  Leftmost Decl: " << leftmostDecl->getDeclKindName() << " "
      //              << leftmostDecl->getNameAsString() << "\n";
      // llvm::outs() << "    SourceRange: " << leftmostDecl->getSourceRange().printToString(*SM) << "\n";
      // llvm::outs() << "   LeftmostOffset: " << leftmostOffset << "\n";
      // llvm::outs() << "   LeftmostNonStarOffset: " << leftmostNonStarLoc << "\n";
      // llvm::outs() << "   SplittingCommaOffsets: " << "\n";
      std::sort(splittingCommaLocs.begin(), splittingCommaLocs.end());
      SourceLocation startloc = leftmostNonStarLoc;
      for (SourceLocation commaLoc : splittingCommaLocs) {
        StringRef declaratorText = Lexer::getSourceText(
          CharSourceRange::getCharRange(startloc, commaLoc), *SM, Ctx->getLangOpts());
        declaratorTexts.push_back(declaratorText.str());
        //llvm::outs() << "    " << startloc << ".." << commaLoc << " => '" << declaratorText << "'\n";
        startloc = commaLoc.getLocWithOffset(1); // move past comma
      }

      
      std::optional<Token> opt_next = Lexer::findNextToken(declEndLoc, *SM, Ctx->getLangOpts());
      SourceLocation afterDeclEndLoc = opt_next.has_value() ?
          opt_next.value().getLocation() : declEndLoc;
      StringRef declaratorText = Lexer::getSourceText(
        CharSourceRange::getCharRange(startloc, afterDeclEndLoc), *SM, Ctx->getLangOpts());
      declaratorTexts.push_back(declaratorText.str());
      //llvm::outs() << "(fin) " << startloc << ".." << afterDeclEndLoc << " => '" << declaratorText << "'\n";



      unsigned declaratorCommas = 0;
      for (const std::string &declaratorText : declaratorTexts) {
        declaratorCommas += std::count(declaratorText.begin(), declaratorText.end(), ',');
      }

      CharSourceRange cr = CharSourceRange::getCharRange(Loc, declEndLoc);
      StringRef overallSpanText = Lexer::getSourceText(cr, *SM, Ctx->getLangOpts());
      unsigned sawCommas = overallSpanText.count(',');
      if (sawCommas != (splittingCommaLocs.size() + declaratorCommas)) {
        curiousDecls["comma_count_mismatch"].push_back(Loc);
      }




      // for (const std::string &declaratorText : declaratorTexts) {        
      //   llvm::outs() << prefixText << " " << declaratorText << "\n";
      // }

      // bool invalid = false;
      // const char* start = SM->getCharacterData(
      //   //SM->getFileLoc(rightmostDecl->getSourceRange().getBegin()), &invalid);
      //   rightmostDecl->getSourceRange().getBegin(), &invalid);
      // if (invalid) {
      //   llvm::outs() << "  getCharacterData invalid for rightmost decl range start\n";
      // } else {
      //   //const char* end = SM->getCharacterData(SM->getFileLoc(declEndLoc), &invalid);
      //   const char* end = SM->getCharacterData(
      //     Lexer::getLocForEndOfToken(declEndLoc, 0, *SM, Ctx->getLangOpts()), &invalid);
      //   if (invalid) {
      //     llvm::outs() << "  getCharacterData invalid for decl end loc\n";
      //   } else {

      //     //StringRef fullText(start, end - start);
      //     llvm::outs() << "=== Full text LENGTH from leftmost decl begin to rightmost decl end: ===\n"
      //                  //<< fullText
      //                  << (end - start)
      //                   << "\n=== End full text ===\n";
      //   }
      // }
    }

    for (auto &[key, Locs] : failedDecls) {
      llvm::outs() << "\n=== Failed decl splits for reason: " << key.str() << " ===\n";
      for (SourceLocation Loc : Locs) {
        llvm::outs() << "  - at " << Loc.printToString(*SM) << "\n";
      }
    }

    for (auto &[key, Locs] : curiousDecls) {
      llvm::outs() << "\n=== Curious decl splits for reason: " << key.str() << " ===\n";
      for (SourceLocation Loc : Locs) {
        llvm::outs() << "  - at " << Loc.printToString(*SM) << "\n";
      }
    }
    


    
    for (auto &[Loc, Decls] : SrcRangeExpStartLocToDeclMap) {
      ProcessedDeclLocs.insert(Loc);
    }
    SrcRangeExpStartLocToDeclMap.clear();
  }

  // backported for LLVM 18, which hadn't implemented this yet
  std::optional<Token> Lexer_findPreviousToken(SourceLocation Loc,
                                               const SourceManager &SM,
                                               const LangOptions &LangOpts,
                                               bool IncludeComments = false) {
                                                SourceLocation macroArgExpStart;
  
  const auto StartOfFile = SM.getLocForStartOfFile(SM.getFileID(Loc));
  
  while (Loc != StartOfFile) {
    Loc = Loc.getLocWithOffset(-1);
    if (Loc.isInvalid())
      return std::nullopt;

    Loc = Lexer::GetBeginningOfToken(Loc, SM, LangOpts);
 
    Token Tok;
    if (Lexer::getRawToken(Loc, Tok, SM, LangOpts)) {
      continue; // Not a token, go to prev location.
    }
    if (!Tok.is(tok::comment) || IncludeComments) {
      return Tok;
    }
  }
  return std::nullopt;
}

private:
  //ExecutionContext &Context;
  SourceManager *SM;
  ASTContext *Ctx;
  
  DenseMap<SourceLocation, std::vector<const NamedDecl*>> SrcRangeExpStartLocToDeclMap;
  DenseSet<SourceLocation> ProcessedDeclLocs;
};


} // end anonymous namespace

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::OptionCategory
    PrintDeclLocsCategory("xj-print-decl-locs options");



int main(int argc, const char **argv) {
  // Standard LLVM boilerplate
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  auto Executor = clang::tooling::createExecutorFromCommandLineArgs(
      argc, argv, PrintDeclLocsCategory);

  if (!Executor) {
    llvm::errs() << llvm::toString(Executor.takeError()) << "\n";
    return 1;
  }

  ast_matchers::MatchFinder Finder;
  PrintDeclLocsCallback Callback(*Executor->get()->getExecutionContext());

  // Add matchers for FieldDecl, TypedefDecl, and VarDecl
  Finder.addMatcher(fieldDecl().bind("fieldDecl"), &Callback);
  Finder.addMatcher(typedefDecl().bind("typedefDecl"), &Callback);
  Finder.addMatcher(varDecl().bind("varDecl"), &Callback);

  // Run the matchers over whatever TU(s) the command line args specified.
  auto Err = Executor->get()->execute(newFrontendActionFactory(&Finder));
  if (Err) {
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
  }
  Executor->get()->getToolResults()->forEachResult(
      [](llvm::StringRef key, llvm::StringRef value) {
        llvm::errs() << "----" << key.str() << "\n" << value.str() << "\n";
      });
  return 0;
}
