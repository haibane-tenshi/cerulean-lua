use codespan_reporting::diagnostic::Diagnostic;
use repr::debug_info::opcode::DebugInfo;
use repr::index::RecipeId;
use repr::opcode::OpCode;

#[derive(Debug, Clone, Copy)]
pub struct MissingRecipe(pub RecipeId);

impl MissingRecipe {
    pub(super) fn into_diagnostic<FileId>(
        self,
        file_id: FileId,
        opcode: OpCode,
        debug_info: Option<DebugInfo>,
    ) -> Diagnostic<FileId>
    where
        FileId: Clone,
    {
        use super::{ExtraDiagnostic, TotalSpan};
        use codespan_reporting::diagnostic::Label;
        use DebugInfo::*;

        let MissingRecipe(recipe_id) = self;
        let have_debug_info = debug_info.is_some();

        let name = opcode.name();
        let mut diag = Diagnostic::bug().with_message(format!(
            "bytecode instruction {name} attempted to load non-existent closure recipe with id={recipe_id}",
        ));

        if let Some(debug_info) = debug_info {
            match (opcode, debug_info) {
                (
                    OpCode::MakeClosure(_),
                    FnExpr {
                        function,
                        total_span,
                    },
                ) => diag.with_label([
                    Label::primary(file_id.clone(), function),
                    Label::secondary(file_id, total_span)
                        .with_message("as part of this function constructor"),
                ]),
                (
                    OpCode::MakeClosure(_),
                    FnDecl {
                        function,
                        total_span,
                        ..
                    },
                ) => {
                    diag.with_label([
                        Label::primary(file_id.clone(), function),
                        Label::secondary(file_id, total_span)
                            .with_message("as part of this function declaration"),
                    ]);
                }
                (_, debug_info) => {
                    diag.with_label([Label::primary(file_id, debug_info.total_span())
                        .with_message("triggered by this")])
                }
            }
        }

        match opcode {
            OpCode::MakeClosure(id) if id == recipe_id => diag.with_compiler_bug_note([
                "the opcode loads closure recipe from the chunk current function is defined in",
            ]),
            OpCode::MakeClosure(id) => diag.with_runtime_bug_note([
                format!("the opcode should load closure recipe with id={id}, but it attempted to load recipe with id={recipe_id}"),
                "likely, this diagnostic is malformed".to_string(),
            ]),
            _ => diag.with_runtime_bug_note([
                format!("instruction {name} doesn't access closure recipes"),
                "likely, this diagnostic is malformed".to_string(),
            ])
        };

        if !have_debug_info {
            diag.no_debug_info();
        }

        diag
    }
}
