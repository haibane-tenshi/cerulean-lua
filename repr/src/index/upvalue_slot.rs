use crate::index_vec::Index;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct UpvalueSlot(pub u32);

#[derive(Debug)]
pub struct UpvalueCountError;

impl Index for UpvalueSlot {
    type Error = UpvalueCountError;
    const MAX: Self = UpvalueSlot(u32::MAX);

    fn try_from(val: usize) -> Result<Self, Self::Error> {
        let inner = val.try_into().map_err(|_| UpvalueCountError)?;
        Ok(UpvalueSlot(inner))
    }

    fn into(self) -> usize {
        self.0.try_into().expect("u32 should fit into usize")
    }
}
