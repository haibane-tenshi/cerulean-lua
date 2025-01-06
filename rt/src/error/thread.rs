use crate::runtime::ThreadId;

use codespan_reporting::diagnostic::Diagnostic;

#[derive(Debug, Clone, Copy)]
pub struct ThreadPanicked(pub ThreadId);

impl ThreadPanicked {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        let ThreadPanicked(thread_id) = self;

        Diagnostic::error().with_message(format!("another thread {:?} panicked", thread_id))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ThreadStatus {
    Active,
    Finished,
    Panicked,
    NotExist,
}

#[derive(Debug, Clone, Copy)]
pub struct ReentryFailure {
    pub thread_id: ThreadId,
    pub status: ThreadStatus,
}

impl ReentryFailure {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;

        let ReentryFailure { thread_id, status } = self;

        match status {
            ThreadStatus::Active => {
                let mut diag = Diagnostic::error().with_message(format!(
                    "attempt to resume already active thread {:?}",
                    thread_id
                ));

                diag.with_help(["Lua does not permit resuming into already active threads"]);

                diag
            }
            ThreadStatus::Finished => {
                let mut diag = Diagnostic::error()
                    .with_message(format!("attempt to resume finished thread {:?}", thread_id));
                diag.with_help([
                    "Lua considers it ill-formed to resume threads that are run to completion",
                ]);

                diag
            }
            ThreadStatus::Panicked => Diagnostic::error()
                .with_message(format!("attempt to resume panicked thread {:?}", thread_id)),
            ThreadStatus::NotExist => Diagnostic::error().with_message(format!(
                "attempt to resume non-existent thread {:?}",
                thread_id
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ThreadError {
    Panicked(ThreadPanicked),
    ReentryFailure(ReentryFailure),
}

impl ThreadError {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        match self {
            ThreadError::Panicked(err) => err.into_diagnostic(),
            ThreadError::ReentryFailure(err) => err.into_diagnostic(),
        }
    }

    pub(crate) fn panic_origin(&self) -> Option<ThreadId> {
        match self {
            ThreadError::Panicked(ThreadPanicked(id)) => Some(*id),
            ThreadError::ReentryFailure(ReentryFailure {
                thread_id,
                status: ThreadStatus::Panicked,
            }) => Some(*thread_id),
            ThreadError::ReentryFailure(_) => None,
        }
    }
}

impl From<ThreadPanicked> for ThreadError {
    fn from(value: ThreadPanicked) -> Self {
        ThreadError::Panicked(value)
    }
}

impl From<ReentryFailure> for ThreadError {
    fn from(value: ReentryFailure) -> Self {
        ThreadError::ReentryFailure(value)
    }
}
