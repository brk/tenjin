use super::*;
use syn::{Expr, Path, Type};

#[derive(Debug, Clone)]
pub struct GuidedType {
    pub pretty: String,
    pub parsed: Type,
}

impl GuidedType {
    pub fn new(pretty: String, parsed: Type) -> Self {
        GuidedType { pretty, parsed }
    }

    pub fn from_str(pretty: &str) -> Self {
        let parsed = syn::parse_str(pretty).expect("Failed to parse type from string");
        GuidedType {
            pretty: pretty.to_string(),
            parsed,
        }
    }

    pub fn from_type(ty: Type) -> Self {
        GuidedType {
            pretty: quote::quote!(#ty).to_string(),
            parsed: ty,
        }
    }
}

pub fn is_known_size_1_type(ty: &Type) -> bool {
    match ty {
        Type::Path(path) => path.qself.is_none() && is_known_size_1_path(&path.path),
        _ => false,
    }
}

pub fn is_path_exactly_1(path: &Path, a: &str) -> bool {
    if path.segments.len() == 1 {
        path.segments[0].ident.to_string().as_str() == a
    } else {
        false
    }
}

fn is_path_exactly_2(path: &Path, a: &str, b: &str) -> bool {
    if path.segments.len() == 2 {
        path.segments[0].ident.to_string().as_str() == a
            && path.segments[1].ident.to_string().as_str() == b
    } else {
        false
    }
}

fn is_known_size_1_path(path: &Path) -> bool {
    match path.segments.len() {
        1 => matches!(
            path.segments[0].ident.to_string().as_str(),
            "u8" | "i8" | "bool" | "char"
        ),
        2 => is_path_exactly_2(path, "libc", "c_char"),
        _ => false,
    }
}

pub fn expr_get_path(expr: &Expr) -> Option<&Path> {
    if let Expr::Path(ref path) = *expr {
        Some(&path.path)
    } else {
        None
    }
}

pub fn expr_is_ident(expr: &Expr, ident: &str) -> bool {
    if let Expr::Path(ref path) = *expr {
        is_path_exactly_1(&path.path, ident)
    } else {
        false
    }
}

/// The given expression is being used in a context expecting a u64 value.
/// Builder::cast_expr() will strip value-preserving casts. Because we know
/// the type imposed by the context, we can also entirely elide casts of
/// compatible integer literals.
/// Examples with expr = x of type i16, and with the implicit `as u64` written explicitly:
///      (x as i8)    as u64 => (x as i8) as u64     # inner cast may change value
///      (x as i32)   as u64 => x as u64             # inner cast is value-preserving
///      (1000 as i8) as u64 => (1000 as i8) as u64  # with large literal, inner cast is not value-preserving
///      (10 as i8)   as u64 => 10                   # with small literal, inner cast is value-preserving
///      1000 as u64         => 1000                 # outer cast is made redundant by known context
pub fn expr_in_u64(expr: Box<Expr>) -> Box<Expr> {
    use crate::translator::mk;
    let cast_box = mk().cast_expr(expr, mk().path_ty(vec!["u64"]));
    // If we end up with a cast of a literal, we can elide the outer cast.
    if let Expr::Cast(ref cast) = *cast_box {
        if let Expr::Lit(ref elit) = *cast.expr {
            if let syn::Lit::Int(ref lit_int) = elit.lit {
                // If the literal is small enough, we can elide the cast
                if lit_int.base10_parse::<u64>().is_ok() {
                    return cast.expr.clone();
                }
            }
        }
    }
    cast_box
}

impl Translation<'_> {
    pub fn recognize_c_assignment_cases(
        &self,
        ctx: ExprContext,
        op: c_ast::BinOp,
        qtype: CQualTypeId,
        lhs: CExprId,
        rhs: CExprId,
        compute_type: Option<CQualTypeId>,
        result_type: Option<CQualTypeId>,
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        self.recognize_c_assignment_string_pop(ctx, op, qtype, lhs, rhs, compute_type, result_type)
    }

    fn strip_integral_cast(&self, expr: CExprId) -> CExprId {
        let kind = &self.ast_context.index(expr).kind;
        if let CExprKind::ImplicitCast(_, inner, CastKind::IntegralCast, _, _) = kind {
            *inner
        } else {
            expr
        }
    }

    pub fn strip_implicit_array_to_pointer_cast(&self, expr: CExprId) -> CExprId {
        let kind = &self.ast_context.index(expr).kind;
        if let CExprKind::ImplicitCast(_, inner, CastKind::ArrayToPointerDecay, _, _) = kind {
            *inner
        } else {
            expr
        }
    }

    fn is_integral_lit(&self, expr: CExprId, val: u64) -> bool {
        let kind = &self.ast_context.index(expr).kind;
        if let CExprKind::Literal(_, clit) = kind {
            match clit {
                CLiteral::Integer(value, _base) => *value == val,
                CLiteral::Character(value) => *value == val,
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn get_string_lit(&self, expr: CExprId) -> Option<&CLiteral> {
        let kind = &self.ast_context.index(expr).kind;
        if let CExprKind::Literal(_, clit) = kind {
            match clit {
                CLiteral::String(_, _) => Some(clit),
                _ => None,
            }
        } else {
            None
        }
    }

    fn c_expr_decl_id(&self, expr: CExprId) -> Option<CDeclId> {
        let kind = &self
            .ast_context
            .index(self.c_strip_implicit_casts(expr))
            .kind;
        if let CExprKind::DeclRef(_, decl_id, _) = kind {
            Some(*decl_id)
        } else {
            None
        }
    }

    fn recognize_c_assignment_string_pop(
        &self,
        ctx: ExprContext,
        op: c_ast::BinOp,
        _qtype: CQualTypeId,
        lhs: CExprId,
        rhs: CExprId,
        _compute_type: Option<CQualTypeId>,
        _result_type: Option<CQualTypeId>,
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        // Code matching
        //     s1[strlen(s1)-1] = '\0';
        //         when s1 is guided to be a String
        // can be translated to
        //     s1.pop();
        // (assuming the assignment expr is in a statement position)
        log::trace!("Recognizing C assignment string pop?...");
        if op != c_ast::BinOp::Assign {
            log::trace!("Not an assignment operator");
            return Ok(None);
        }

        if !self.is_integral_lit(self.strip_integral_cast(rhs), 0) {
            log::trace!("Not assigned an integral literal 0");
            return Ok(None);
        }

        let lhs_kind = &self.ast_context.index(lhs).kind;
        if let CExprKind::ArraySubscript(_, raw_base, index, _lrvalue) = lhs_kind {
            if let CExprKind::Binary(
                _sub_cq,
                c_ast::BinOp::Subtract,
                sub_lhs,
                sub_rhs,
                _opt_cq_lhs,
                _opt_cq_rhs,
            ) = self.ast_context.index(*index).kind
            {
                if !self.is_integral_lit(self.strip_integral_cast(sub_rhs), 1) {
                    log::trace!("Not subtracting 1 from the index");
                    return Ok(None);
                }

                if let Some(base_decl_id) = self.c_expr_decl_id(*raw_base) {
                    let guided_type = self
                        .parsed_guidance
                        .borrow_mut()
                        .query_decl_type(self, base_decl_id);

                    if guided_type.is_none_or(|g| g.pretty != "String") {
                        log::trace!("target variable not guided to be of type String");
                        return Ok(None);
                    }
                } else {
                    return Ok(None);
                };

                if let CExprKind::Call(_, callee_raw, args) = &self.ast_context.index(sub_lhs).kind
                {
                    let callee = self.c_strip_implicit_casts(*callee_raw);
                    if let CExprKind::DeclRef(_, callee_decl_id, _opt_cq) =
                        self.ast_context.index(callee).kind
                    {
                        if let CDeclKind::Function { name, .. } =
                            &self.ast_context[callee_decl_id].kind
                        {
                            if name != "strlen" {
                                return Ok(None);
                            }

                            if args.len() == 1
                                && self.c_expr_decl_id(args[0]) == self.c_expr_decl_id(*raw_base)
                            {
                                let tgt_expr =
                                    self.convert_expr(ctx, self.c_strip_implicit_casts(*raw_base))?;
                                let pop_call = mk().method_call_expr(
                                    tgt_expr.to_expr(),
                                    mk().path_segment("pop"),
                                    vec![],
                                );
                                log::trace!("Recognized assignment, returning pop call");
                                return Ok(Some(WithStmts::new_val(pop_call)));
                            } else {
                                log::trace!("Call to strlen does not match the expected argument");
                            }
                        }
                        log::trace!("Not a call to strlen");
                    } else {
                        log::trace!(
                            "Callee is not a decl ref: {:?}",
                            self.ast_context.index(callee).kind
                        );
                    }
                }
            }
        }
        log::trace!("LHS not an array subscript");
        Ok(None)
    }

    #[allow(clippy::vec_box)]
    fn call_form_cases(
        &self,
        func: Box<Expr>,
        args: Vec<Box<Expr>>,
        cargs: &[CExprId],
    ) -> RecognizedCallForm {
        if tenjin::expr_is_ident(&func, "printf") {
            return RecognizedCallForm::PrintfOut(args, cargs[0]);
        }

        if tenjin::expr_is_ident(&func, "fprintf") && !args.is_empty() {
            if tenjin::expr_is_ident(&args[0], "stderr")
                || tenjin::expr_is_ident(&args[0], "__stderrp")
            {
                return RecognizedCallForm::PrintfErr(args[1..].to_vec(), cargs[1]);
            }
            if tenjin::expr_is_ident(&args[0], "stdout")
                || tenjin::expr_is_ident(&args[0], "__stdoutp")
            {
                return RecognizedCallForm::PrintfOut(args[1..].to_vec(), cargs[1]);
            }
        }

        RecognizedCallForm::OtherCall(func, args)
    }

    #[allow(clippy::vec_box)]
    pub fn convert_call_with_args(
        &self,
        func: Box<Expr>,
        args: Vec<Box<Expr>>,
        cargs: &[CExprId],
    ) -> Box<Expr> {
        match self.call_form_cases(func, args, cargs) {
            RecognizedCallForm::PrintfOut(args, fmt_carg) => {
                let fmt_string_span = self
                    .ast_context
                    .display_loc(&self.ast_context[fmt_carg].loc);
                mk().mac_expr(refactor_format::build_format_macro(
                    self,
                    "print",
                    "println",
                    &args,
                    cargs,
                    None,
                    fmt_string_span,
                ))
            }
            RecognizedCallForm::PrintfErr(args, fmt_carg) => {
                let fmt_string_span = self
                    .ast_context
                    .display_loc(&self.ast_context[fmt_carg].loc);
                mk().mac_expr(refactor_format::build_format_macro(
                    self,
                    "eprint",
                    "eprintln",
                    &args,
                    cargs,
                    None,
                    fmt_string_span,
                ))
            }
            RecognizedCallForm::OtherCall(func, args) => mk().call_expr(func, args),
        }
    }

    #[allow(clippy::borrowed_box)]
    pub fn call_form_cases_preconversion(
        &self,
        ctx: ExprContext,
        func: &Box<Expr>,
        cargs: &[CExprId],
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        if let Some(path) = tenjin::expr_get_path(func) {
            match () {
                _ if tenjin::is_path_exactly_1(path, "fputs") => {
                    self.recognize_preconversion_call_fputs_stdout_guided(ctx, func, cargs)
                }
                _ if tenjin::is_path_exactly_1(path, "fgets") => {
                    self.recognize_preconversion_call_fgets_stdin(ctx, func, cargs)
                }
                _ if tenjin::is_path_exactly_1(path, "strlen") => {
                    self.recognize_preconversion_call_strlen_guided(ctx, func, cargs)
                }
                _ if tenjin::is_path_exactly_1(path, "strcspn") => {
                    self.recognize_preconversion_call_strcspn_guided(ctx, func, cargs)
                }
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    #[allow(clippy::borrowed_box)]
    fn recognize_preconversion_call_fputs_stdout_guided(
        &self,
        ctx: ExprContext,
        func: &Box<Expr>,
        cargs: &[CExprId],
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        if tenjin::expr_is_ident(func, "fputs") && cargs.len() == 2 {
            // fputs(FOO, stdout)
            //    when FOO is a simple variable with type String
            // should be translated to
            // println!(FOO)
            if !(self.c_expr_is_var_ident(cargs[1], "stdout")
                || self.c_expr_is_var_ident(cargs[1], "__stdoutp"))
            {
                return Ok(None);
            }

            if let Some(var_cdecl_id) = self.c_expr_get_var_decl_id(cargs[0]) {
                if self
                    .parsed_guidance
                    .borrow_mut()
                    .query_decl_type(self, var_cdecl_id)
                    .is_some_and(|g| g.pretty == "String")
                {
                    let expr = self.convert_expr(ctx.used(), cargs[0])?;
                    let print_call = mk().mac_expr(refactor_format::build_format_macro_from(
                        self,
                        "%s".to_string(),
                        "print",
                        "println",
                        &[expr.to_expr()],
                        &[cargs[0]],
                        None,
                        None,
                        self.ast_context
                            .display_loc(&self.ast_context[cargs[0]].loc),
                    ));
                    return Ok(Some(WithStmts::new_val(print_call)));
                }
            }
        }

        Ok(None)
    }

    #[allow(clippy::borrowed_box)]
    fn recognize_preconversion_call_strlen_guided(
        &self,
        ctx: ExprContext,
        func: &Box<Expr>,
        cargs: &[CExprId],
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        if tenjin::expr_is_ident(func, "strlen") && cargs.len() == 1 {
            // strlen(FOO)
            //    when FOO is a simple variable with type String
            // should be translated to
            // FOO.len()
            if let Some(var_cdecl_id) = self.c_expr_get_var_decl_id(cargs[0]) {
                if self
                    .parsed_guidance
                    .borrow_mut()
                    .query_decl_type(self, var_cdecl_id)
                    .is_some_and(|g| g.pretty == "String")
                {
                    let expr = self.convert_expr(ctx.used(), cargs[0])?;
                    let len_call = mk().method_call_expr(expr.to_expr(), "len", vec![]);
                    return Ok(Some(WithStmts::new_val(len_call)));
                }
            }
        }

        Ok(None)
    }

    #[allow(clippy::borrowed_box)]
    fn recognize_preconversion_call_strcspn_guided(
        &self,
        ctx: ExprContext,
        func: &Box<Expr>,
        cargs: &[CExprId],
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        if tenjin::expr_is_ident(func, "strcspn") && cargs.len() == 2 {
            // strcspn(FOO, BAR)
            //    when FOO is a simple variable with type String
            //    and BAR is a simple variable with type String
            // should be translated to
            // strcspn_str(&FOO, &BAR)
            if let (Some(var_cdecl_id_foo), Some(var_cdecl_id_bar)) = (
                self.c_expr_get_var_decl_id(cargs[0]),
                self.c_expr_get_var_decl_id(cargs[1]),
            ) {
                if self
                    .parsed_guidance
                    .borrow_mut()
                    .query_decl_type(self, var_cdecl_id_foo)
                    .is_some_and(|g| g.pretty == "String")
                    && self
                        .parsed_guidance
                        .borrow_mut()
                        .query_decl_type(self, var_cdecl_id_bar)
                        .is_some_and(|g| g.pretty == "String")
                {
                    self.with_cur_file_item_store(|item_store| {
                        item_store.add_item_str_once("fn strcspn_str(s: &str, chars: &str) -> usize { s.chars().take_while(|c| !chars.contains(*c)).count() }",
                        );
                    });

                    let expr_foo = self.convert_expr(ctx.used(), cargs[0])?;
                    let expr_bar = self.convert_expr(ctx.used(), cargs[1])?;
                    let strcspn_call = mk().call_expr(
                        mk().path_expr(vec!["strcspn_str"]),
                        vec![
                            mk().addr_of_expr(expr_foo.to_expr()),
                            mk().addr_of_expr(expr_bar.to_expr()),
                        ],
                    );
                    return Ok(Some(WithStmts::new_val(strcspn_call)));
                }
            }
        }

        Ok(None)
    }

    #[allow(clippy::borrowed_box)]
    fn recognize_preconversion_call_fgets_stdin(
        &self,
        ctx: ExprContext,
        func: &Box<Expr>,
        cargs: &[CExprId],
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        if tenjin::expr_is_ident(func, "fgets") && cargs.len() == 3 {
            // fgets(FOO, limit_expr, stdin)
            //    when FOO is a simple variable with type String
            // should be translated to
            // io::stdin().lock().take(limit_expr - 1).read_line(&mut FOO).unwrap();
            //
            // Because take() expects a u64, we may be able to elide unnecessary casts from limit_expr.
            //
            // One might think the `.take()` is unnecessary, since the C code needed the limit to
            // avoid a memory safety violation. But the limit_expr also serves to place a bound on the
            // period spent blocking on the read. If the stream produces limit+1 bytes then blocks,
            // the fgets() call would return before blocking,
            // and the .take() is what stops Rust from blocking.

            if !(self.c_expr_is_var_ident(cargs[2], "stdin")
                || self.c_expr_is_var_ident(cargs[2], "__stdinp"))
            {
                return Ok(None);
            }

            // XREF:TENJIN-GUIDANCE-STRAWMAN
            if let Some(var_cdecl_id) = self.c_expr_get_var_decl_id(cargs[0]) {
                if self
                    .parsed_guidance
                    .borrow_mut()
                    .query_decl_type(self, var_cdecl_id)
                    .is_some_and(|g| g.pretty == "String")
                {
                    self.with_cur_file_item_store(|item_store| {
                        item_store.add_use(vec!["std".into(), "io".into()], "Read");
                        item_store.add_use(vec!["std".into(), "io".into()], "BufRead");
                    });

                    let dst = self.convert_expr(ctx.used(), cargs[0])?;
                    let ref_mut_dst = mk().mutbl().addr_of_expr(dst.to_expr());
                    let expr = self.convert_expr(ctx.used(), cargs[1])?;
                    let expr_u64 = tenjin::expr_in_u64(expr.to_expr());
                    let std_io_path: Box<Expr> = mk().path_expr(vec!["std", "io", "stdin"]);
                    let stdin_call = mk().call_expr(std_io_path, vec![]);
                    let lock_call = mk().method_call_expr(stdin_call.clone(), "lock", vec![]);
                    let take_call = mk().method_call_expr(lock_call, "take", vec![expr_u64]);
                    let read_line =
                        mk().method_call_expr(take_call, "read_line", vec![ref_mut_dst]);
                    let unwrap_call = mk().method_call_expr(read_line, "unwrap", vec![]);
                    return Ok(Some(WithStmts::new_val(unwrap_call)));
                }
            }
        }
        Ok(None)
    }
}
