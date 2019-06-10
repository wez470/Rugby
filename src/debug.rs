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

pub fn u16_to_hex(n: u16) -> String {
    hex::encode_upper(vec![(n >> 8) as u8, n as u8])
}

pub fn u8_to_hex(n: u8) -> String {
    hex::encode_upper(vec![n])
}