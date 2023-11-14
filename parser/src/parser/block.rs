use crate::parser::prelude::*;

pub(crate) fn block<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Option<Spanned<()>>,
    Success = CompleteOr<ParseFailure>,
    Failure = Never,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.scope();

        let state = inner_block(frag.new_core()).parse_once(s)?.inspect(|_| {
            frag.commit();
        });

        Ok(state)
    }
}

pub(crate) fn inner_block<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Option<Spanned<()>>,
    Success = CompleteOr<ParseFailure>,
    Failure = Never,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::stmt::return_::return_;
        use crate::parser::stmt::statement;

        let mut frag = core.decl();

        let source = s.source();
        let _span = trace_span!("block").entered();

        let statement = |s: Lexer<'s>| statement(frag.new_core()).parse_once(s);

        let state = statement
            .repeat_with(discard)
            .optional()
            .parse_once(s)?
            .and(
                |s: Lexer<'s>| -> Result<_, FailFast> {
                    let state = return_(frag.new_core())
                        .optional()
                        .parse_once(s.clone())?
                        .map_success(|success| match success {
                            CompleteOr::Complete(Complete) => CompleteOr::Complete(Complete),
                            CompleteOr::Other(_) => {
                                let mut s = s;
                                let _ = s.next_token();

                                let err: ParseFailure = ParseCause::ExpectedStmt(s.span()).into();

                                CompleteOr::Other(err)
                            }
                        });

                    Ok(state)
                },
                |t, u| match (t, u) {
                    (None, None) => None,
                    (t, None) | (None, t) => t,
                    (Some(t), Some(u)) => Some(discard(t, u)),
                },
            )?
            .inspect(move |output| {
                frag.commit();

                match output {
                    Some(output) => trace!(span=?output.span(), str=&source[output.span()]),
                    None => trace!(str = ""),
                };
            });

        Ok(state)
    }
}
