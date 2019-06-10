use crate::cpu::registers::{Reg8, Reg16};
/// Represents a watchable component
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Watch {
    /// A single memory address
    Mem(u16),
    /// A range of memory addresses
    MemRange(u16, u16),
    /// An 8-bit register
    Reg8(Reg8),
    /// A 16-bit register
    Reg16(Reg16),
}