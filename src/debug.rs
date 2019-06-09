/// Represents a watchable component
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Watch {
    /// A single memory address
    Mem(u16),
    /// A range of memory addresses
    MemRange(u16, u16),
}