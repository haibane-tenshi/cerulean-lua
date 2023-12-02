use repr::chunk::{Function, FunctionDebugInfo};
use repr::index::FunctionId;
use repr::tivec::TiVec;

#[derive(Debug, Default)]
pub struct FuncTable {
    functions: TiVec<FunctionId, Function>,
    debug_info: TiVec<FunctionId, FunctionDebugInfo>,
}

impl FuncTable {
    // pub fn new() -> Self {
    //     Default::default()
    // }

    pub fn with_script() -> Self {
        let dummy = FunctionDebugInfo {
            name: "".to_string(),
            span: 0..0,
            opcodes: Default::default(),
        };

        FuncTable {
            functions: vec![Default::default()].into(),
            debug_info: vec![dummy].into(),
        }
    }

    pub fn view(&mut self) -> FuncTableView {
        FuncTableView::new(self)
    }

    pub fn resolve(
        self,
    ) -> (
        TiVec<FunctionId, Function>,
        TiVec<FunctionId, FunctionDebugInfo>,
    ) {
        (self.functions, self.debug_info)
    }

    fn inner_state(&self) -> InnerState {
        InnerState {
            functions: self.functions.next_key(),
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState { functions } = state;

        self.functions.truncate(functions.into());
        self.debug_info.truncate(functions.into());
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

    pub fn push(&mut self, func: Function, debug_info: FunctionDebugInfo) -> FunctionId {
        let func_id = self.func_table.functions.push_and_get_key(func);
        let debug_info_id = self.func_table.debug_info.push_and_get_key(debug_info);

        debug_assert_eq!(func_id, debug_info_id);

        func_id
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
