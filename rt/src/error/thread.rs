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
    Finished,
    Panicked,
}

#[derive(Debug, Clone, Copy)]
pub struct ResumeDeadThread {
    pub thread_id: ThreadId,
    pub status: ThreadStatus,
}

impl ResumeDeadThread {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        use super::ExtraDiagnostic;

        let ResumeDeadThread { thread_id, status } = self;

        match status {
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
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ThreadError {
    Panicked(ThreadPanicked),
    ResumeDead(ResumeDeadThread),
}

impl ThreadError {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        match self {
            ThreadError::Panicked(err) => err.into_diagnostic(),
            ThreadError::ResumeDead(err) => err.into_diagnostic(),
        }
    }

    pub(crate) fn panic_origin(&self) -> Option<ThreadId> {
        match self {
            ThreadError::Panicked(ThreadPanicked(id)) => Some(*id),
            ThreadError::ResumeDead(ResumeDeadThread {
                thread_id,
                status: ThreadStatus::Panicked,
            }) => Some(*thread_id),
            ThreadError::ResumeDead(ResumeDeadThread {
                status: ThreadStatus::Finished,
                ..
            }) => None,
        }
    }
}

impl From<ThreadPanicked> for ThreadError {
    fn from(value: ThreadPanicked) -> Self {
        ThreadError::Panicked(value)
    }
}

impl From<ResumeDeadThread> for ThreadError {
    fn from(value: ResumeDeadThread) -> Self {
        ThreadError::ResumeDead(value)
    }
}
