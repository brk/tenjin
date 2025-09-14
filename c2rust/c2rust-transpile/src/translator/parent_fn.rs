//! Compute a map from local variable/parameter `CDeclId`s to the `CDeclId` of
//! their parent function.

use crate::c_ast::iterators::SomeId;
use crate::c_ast::{iterators::DFExpr, CDeclId, CDeclKind, CStmtId, TypedAstContext};
use std::collections::HashMap;

struct ParentFnCollector<'a> {
    ast_context: &'a TypedAstContext,
    parent_fn_map: HashMap<CDeclId, CDeclId>,
    current_fn: Option<CDeclId>,
}

impl<'a> ParentFnCollector<'a> {
    fn new(ast_context: &'a TypedAstContext) -> Self {
        ParentFnCollector {
            ast_context,
            parent_fn_map: HashMap::new(),
            current_fn: None,
        }
    }

    fn visit_function_body(&mut self, body_id: CStmtId) {
        let iter = DFExpr::new(self.ast_context, SomeId::Stmt(body_id));
        for node in iter {
            if let SomeId::Decl(decl_id) = node {
                let decl = &self.ast_context[decl_id];
                if let CDeclKind::Variable {
                    has_global_storage, ..
                } = decl.kind
                {
                    if let Some(fn_id) = self.current_fn {
                        assert!(!has_global_storage);
                        self.parent_fn_map.insert(decl_id, fn_id);
                    }
                }
            }
        }
    }
}

pub fn compute_parent_fn_map(ast_context: &TypedAstContext) -> HashMap<CDeclId, CDeclId> {
    let mut collector = ParentFnCollector::new(ast_context);
    for &decl_id in ast_context.c_decls_top.iter() {
        let decl = &ast_context[decl_id];
        if let CDeclKind::Function {
            ref parameters,
            body: Some(body_id),
            ..
        } = decl.kind
        {
            collector.current_fn = Some(decl_id);
            for param_id in parameters {
                collector.parent_fn_map.insert(*param_id, decl_id);
            }
            collector.visit_function_body(body_id);
            collector.current_fn = None;
        }
    }
    collector.parent_fn_map
}
