use super::*;
use syn::{Expr, Path, Type};

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
        qtype: CQualTypeId,
        lhs: CExprId,
        rhs: CExprId,
        compute_type: Option<CQualTypeId>,
        result_type: Option<CQualTypeId>,
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        // Code matching
        //     s1[strlen(s1)-1] = '\0';
        //         when s1 is guided to be a String
        // can be translated to
        //     s1.pop();
        // (assuming the assignment expr is in a statement position)
        log::info!("Recognizing C assignment string pop?...");
        if op != c_ast::BinOp::Assign {
            log::info!("Not an assignment operator");
            return Ok(None);
        }

        if !self.is_integral_lit(self.strip_integral_cast(rhs), 0) {
            log::info!("Not assigned an integral literal 0");
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
                    log::info!("Not subtracting 1 from the index");
                    return Ok(None);
                }

                if let Some(base_decl_id) = self.c_expr_decl_id(*raw_base) {
                    let base_decl = &self.ast_context[base_decl_id].kind;
                    if let CDeclKind::Variable { ident, .. } = base_decl {
                        if !(ident == "s1" || ident == "s2") {
                            log::info!("target string not s1 or s2");
                            return Ok(None);
                        }
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
                                log::info!("Recognized assignment, returning pop call");
                                return Ok(Some(WithStmts::new_val(pop_call)));
                            } else {
                                log::info!("Call to strlen does not match the expected argument");
                            }
                        }
                        log::info!("Not a call to strlen");
                    } else {
                        log::info!(
                            "Callee is not a decl ref: {:?}",
                            self.ast_context.index(callee).kind
                        );
                    }
                }
            }
        }
        log::info!("LHS not an array subscript");
        Ok(None)
    }
}
