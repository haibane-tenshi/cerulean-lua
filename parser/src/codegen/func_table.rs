use repr::chunk::Function;
use repr::index::{FunctionCapacityError, FunctionId};
use repr::index_vec::IndexVec;

#[derive(Debug, Default)]
pub struct FuncTable {
    functions: IndexVec<FunctionId, Function>,
}

impl FuncTable {
    // pub fn new() -> Self {
    //     Default::default()
    // }

    pub fn with_script() -> Self {
        FuncTable {
            functions: vec![Default::default()].try_into().unwrap(),
        }
    }

    pub fn view(&mut self) -> FuncTableView {
        FuncTableView::new(self)
    }

    pub fn resolve(self) -> IndexVec<FunctionId, Function> {
        self.functions
    }

    fn inner_state(&self) -> InnerState {
        InnerState {
            functions: self.functions.len(),
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState { functions } = state;

        self.functions.truncate(functions);
    }
}

#[derive(Debug, Clone, Copy)]
struct InnerState {
    functions: FunctionId,
}

#[derive(Debug)]
pub struct FuncTableView<'a> {
    func_table: &'a mut FuncTable,
    inner_state: InnerState,
}

impl<'a> FuncTableView<'a> {
    pub fn new(func_table: &'a mut FuncTable) -> Self {
        let inner_state = func_table.inner_state();

        FuncTableView {
            func_table,
            inner_state,
        }
    }

    pub fn borrow(&mut self) -> &mut FuncTable {
        self.func_table
    }

    pub fn push(&mut self, func: Function) -> Result<FunctionId, FunctionCapacityError> {
        self.func_table.functions.push(func)
    }

    pub fn commit(self) {
        std::mem::forget(self)
    }
}

impl<'a> Drop for FuncTableView<'a> {
    fn drop(&mut self) {
        let FuncTableView {
            func_table,
            inner_state,
        } = self;

        func_table.apply(*inner_state)
    }
}
