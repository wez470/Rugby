use log::warn;
use crate::cpu::{Reg8, Reg16};

// The below tables are based on the tables at
// http://pastraiser.com/cpu/gameboy/gameboy_opcodes.html.

/// The number of cycles taken by each instruction, ignoring any conditional actions (see
/// `CONDITIONAL_CYCLES` below).
///
/// `0xCB`-prefixed instructions are handled by a separate table.
pub static BASE_CYCLES: [usize; 0x100] = [
//   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
     4, 12,  8,  8,  4,  4,  8,  4, 20,  8,  8,  8,  4,  4,  8,  4, // 0
     4, 12,  8,  8,  4,  4,  8,  4, 12,  8,  8,  8,  4,  4,  8,  4, // 1
     8, 12,  8,  8,  4,  4,  8,  4,  8,  8,  8,  8,  4,  4,  8,  4, // 2
     8, 12,  8,  8, 12, 12, 12,  4,  8,  8,  8,  8,  4,  4,  8,  4, // 3
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 4
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 5
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 6
     8,  8,  8,  8,  8,  8,  4,  8,  4,  4,  4,  4,  4,  4,  8,  4, // 7
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 8
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 9
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // A
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // B
     8, 12, 12, 16, 12, 16,  8, 16,  8, 16, 12,  0, 12, 24,  8, 16, // C
     8, 12, 12,  0, 12, 16,  8, 16,  8, 16, 12,  0, 12,  0,  8, 16, // D
    12, 12,  8,  0,  0, 16,  8, 16, 16,  4, 16,  0,  0,  0,  8, 16, // E
    12, 12,  8,  4,  0, 16,  8, 16, 12,  8, 16,  4,  0,  0,  8, 16, // F
];

/// Cycles that get added if a conditional action is taken.
///
/// For example, `JP Z, a16` will have cycles added based on this table only if the zero flag is
/// set, which causes it to actually jump.
pub static CONDITIONAL_CYCLES: [usize; 0x100] = [
//   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 0
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 1
     4,  0,  0,  0,  0,  0,  0,  0,  4,  0,  0,  0,  0,  0,  0,  0, // 2
     4,  0,  0,  0,  0,  0,  0,  0,  4,  0,  0,  0,  0,  0,  0,  0, // 3
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 4
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 5
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 6
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 7
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 8
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 9
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // A
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // B
    12,  0,  4,  0, 12,  0,  0,  0, 12,  0,  4,  0, 12,  0,  0,  0, // C
    12,  0,  4,  0, 12,  0,  0,  0, 12,  0,  4,  0, 12,  0,  0,  0, // D
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // E
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // F
];

/// The number of cycles taken by each `0xCB`-prefixed instruction. These are never conditional.
///
/// NOTE: The original reference we used claimed that `BIT n, (HL)` instructions take 16 ticks, but
/// the blargg instr_timing test and Mooneye's [accuracy doc] confirm they actually take 12. This
/// makes more sense, anyway:
///
///     * 4 cycles to read the 0xCB prefix
///     * 4 cycles to read the opcode
///     * 4 cycles to read (HL)
///     * Unlike `RES n, (HL)` or `SET n, (HL)` there is nothing to write back to (HL).
///
/// [accuracy doc]: https://github.com/Gekkio/mooneye-gb/blob/master/docs/accuracy.markdown
pub static PREFIX_CB_BASE_CYCLES: [usize; 0x100] = [
//   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 0
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 1
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 2
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 3
     8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8, // 4
     8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8, // 5
     8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8, // 6
     8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8, // 7
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 8
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 9
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // A
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // B
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // C
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // D
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // E
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // F
];
/// The length of every instruction. (1 byte for the opcode plus optional bytes for operands.)
///
/// Every `0xCB`-prefixed instruction has length 2 (counting the `CB` opcode), so we handle that
/// simply by putting a 2 in the 0xCB location.
///
/// NOTE: The original reference we used claimed 0xE2 and 0xF2 had length 2, but empirically they
/// seem to be length 1.
pub static INSTRUCTION_LENGTH: [usize; 0x100] = [
//   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
     1,  3,  1,  1,  1,  1,  2,  1,  3,  1,  1,  1,  1,  1,  2,  1, // 0
     2,  3,  1,  1,  1,  1,  2,  1,  2,  1,  1,  1,  1,  1,  2,  1, // 1
     2,  3,  1,  1,  1,  1,  2,  1,  2,  1,  1,  1,  1,  1,  2,  1, // 2
     2,  3,  1,  1,  1,  1,  2,  1,  2,  1,  1,  1,  1,  1,  2,  1, // 3
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 4
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 5
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 6
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 7
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 8
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 9
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // A
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // B
     1,  1,  3,  3,  3,  1,  2,  1,  1,  1,  3,  2,  3,  3,  2,  1, // C
     1,  1,  3,  1,  3,  1,  2,  1,  1,  1,  3,  1,  3,  1,  2,  1, // D
     2,  1,  1,  1,  1,  1,  2,  1,  2,  1,  3,  1,  1,  1,  2,  1, // E
     2,  1,  1,  1,  1,  1,  2,  1,  2,  1,  3,  1,  1,  1,  2,  1, // F
];

/// The largest value in the `INSTRUCTION_LENGTH` table.
pub const MAX_INSTRUCTION_LENGTH: usize = 3;

/// Represents an operand resolving to an 8-bit value.
#[derive(Clone, Copy, Debug)]
pub enum Operand8 {
    /// 8-bit immediate value.
    Imm8(u8),

    /// 8-bit register.
    Reg8(Reg8),

    /// Memory location at the given immediate address.
    MemImm(u16),

    /// Memory location at the address in the given register.
    MemReg(Reg16),

    /// Memory location at the address `0xFF00 + byte`.
    MemHighImm(u8),

    /// Memory location at the address `0xFF00 + C`. Used only in `LD A, (0xFF00+C)` and
    /// `LD (0xFF00+C), A`.
    MemHighC,

    /// Memory location at the address in the HL register. HL is incremented after accessing this
    /// operand. This is used in `LD A, (HL+)` and `LD (HL+), A`.
    MemHlPostInc,

    /// Memory location at the address in the HL register. HL is decremented after accessing this
    /// operand. This is used in `LD A, (HL-)` and `LD (HL-), A`.
    MemHlPostDec,
}

/// Represents an operand resolving to a 16-bit value.
#[derive(Clone, Copy, Debug)]
pub enum Operand16 {
    /// 16-bit immediate value.
    Imm16(u16),

    /// 16-bit register.
    Reg16(Reg16),

    /// 16-bit memory location at the given immediate address (and the byte above).
    MemImm16(u16),
}

/// Represents the condition checked by a conditional instruction (JP, JR, RET, or CALL).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Cond {
    /// Unconditional.
    None,

    /// Z: zero flag is set.
    Zero,

    /// NZ: zero flag is unset.
    NotZero,

    /// C: carry flag is set.
    Carry,

    /// NC: carry flag is unset.
    NotCarry,
}

#[derive(Debug)]
pub enum Inst {
    /// `NOP`: No operation.
    Nop,

    /// `STOP`: Halt CPU & LCD display until button pressed.
    Stop,

    /// `HALT`: Power down CPU until an interrupt occurs. The Game Boy uses this to save power.
    Halt,

    /// `DI`: Disable interrupts.
    Di,

    /// `EI`: Enable interrupts.
    Ei,

    /// `JP cond?, xx`: Absolute jump. Optionally conditional.
    Jp(Operand16, Cond),

    /// `JR cond?, x`: Relative jump. Optionally conditional.
    Jr(i8, Cond),

    /// `CALL cond?, addr`: Call the function at the given immediate address. Optionally
    /// conditional.
    Call(u16, Cond),

    /// `RST addr`: Call one of eight "restarts" stored in the first 256 bytes of memory. Works
    /// just like a function call to the given address. We store only the low byte of the address;
    /// the high byte is always 0x00.
    ///
    /// Valid addresses: 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, and 0x38.
    Rst(u8),

    /// `RET cond?`: Return from the current function by jumping to an address popped from the
    /// stack. Optionally conditional.
    Ret(Cond),

    /// `RETI`: Return from the current function, like `RET`, but also enable interrupts. Typically
    /// used to return from interrupt handlers because interrupts are disabled when they are
    /// triggered.
    Reti,

    /// `PUSH xx`: Push the given register onto the stack.
    Push(Reg16),

    /// `POP xx`: Pop the top of the stack into the given register.
    Pop(Reg16),

    /// `LD x, x`: 8-bit loads, stores, and moves.
    Ld8(Operand8, Operand8),

    /// `LD xx, xx`: 16-bit loads, stores, and moves.
    Ld16(Operand16, Operand16),

    /// `LD HL, SP+x`: Add signed 8-bit immediate to stack pointer and store the result in HL.
    LdHlSp(i8),

    /// `INC x`: 8-bit increment.
    Inc8(Operand8),

    /// `DEC x`: 8-bit decrement.
    Dec8(Operand8),

    /// `INC xx`: 16-bit increment.
    Inc16(Operand16),

    /// `DEC xx`: 16-bit decrement.
    Dec16(Operand16),

    /// `ADD A, x`: 8-bit addition. Implicit first operand is always register A.
    AddA(Operand8),

    /// `ADD HL, xx`: 16-bit addition. Implicit first operand is always register HL.
    AddHl(Operand16),

    /// `ADD SP, x`: Add signed 8-bit immediate to stack pointer.
    AddSp(i8),

    /// `ADC A, x`: Addition with carry. Implicit first operand is always register A.
    AdcA(Operand8),

    /// `SUB x`: Subtraction. Implicit first operand is always register A.
    Sub(Operand8),

    /// `SBC A, x`: Subtraction with carry. Implicit first operand is always register A.
    SbcA(Operand8),

    /// `AND x`: Bitwise and. Implicit first operand is always register A.
    And(Operand8),

    /// `XOR x`: Bitwise exclusive or. Implicit first operand is always register A.
    Xor(Operand8),

    /// `OR x`: Bitwise or. Implicit first operand is always register A.
    Or(Operand8),

    /// `CP x`: Comparison. Implicit first operand is always register A. Operates like subtraction
    /// but only for the flag side effects.
    Cp(Operand8),

    /// `RLC x`: 1-bit rotate left circular. Bits move according to the diagram, where 0 is the low
    /// bit, 7 is the high bit, and `C` is the carry flag.
    ///
    /// ```text
    ///         +--------------+
    /// +---+   |  +--------+  |
    /// | C |<--+--| 7 <- 0 |<-+
    /// +---+      +--------+
    /// ```
    Rlc(Operand8),

    /// `RL x`: 1-bit rotate left, through the carry flag. Bits move according to the diagram,
    /// where 0 is the low bit, 7 is the high bit, and `C` is the carry flag.
    ///
    /// ```text
    ///   +---------------------+
    ///   | +---+   +--------+  |
    ///   +-| C |<--| 7 <- 0 |<-+
    ///     +---+   +--------+
    /// ```
    Rl(Operand8),

    /// `RRC x`: 1-bit rotate right circular. Bits move according to the diagram, where 0 is the low
    /// bit, 7 is the high bit, and `C` is the carry flag.
    ///
    /// ```text
    /// +--------------+
    /// |  +--------+  |   +---+
    /// +->| 7 -> 0 |--+-->| C |
    ///    +--------+      +---+
    /// ```
    Rrc(Operand8),

    /// `RR x`: 1-bit rotate right, through the carry flag. Bits move according to the diagram,
    /// where 0 is the low bit, 7 is the high bit, and `C` is the carry flag.
    ///
    /// ```text
    ///   +---------------------+
    ///   |  +--------+   +---+ |
    ///   +->| 7 -> 0 |-->| C |-+
    ///      +--------+   +---+
    /// ```
    Rr(Operand8),

    /// `RLCA`: Shorter encoding of `RLC A`. It also affects the zero flag differently.
    Rlca,

    /// `RLA`: Shorter encoding of `RL A`. It also affects the zero flag differently.
    Rla,

    /// `RRCA`: Shorter encoding of `RRC A`. It also affects the zero flag differently.
    Rrca,

    /// `RRA`: Shorter encoding of `RR A`. It also affects the zero flag differently.
    Rra,

    /// `SLA x`: 1-bit arithmetic shift left.
    ///   * low bit = 0
    ///   * carry = old high bit
    Sla(Operand8),

    /// `SRA x`: 1-bit arithmetic shift right.
    ///   * high bit unchanged (i.e. result has the same sign)
    ///   * carry = old low bit
    Sra(Operand8),

    /// `SRL x`: 1-bit logical shift right.
    ///   * high bit = 0
    ///   * carry = old low bit
    Srl(Operand8),

    /// `SWAP x`: Swap high and low nibbles (4 bits).
    Swap(Operand8),

    /// `BIT b, x`: Test the bit at the given index in the given operand. Sets the zero flag
    /// accordingly.
    Bit(u8, Operand8),

    /// `RES b, x`: Reset the bit at the given index in the given operand. (Set to zero.)
    Res(u8, Operand8),

    /// `SET b, x`: Set the bit at the given index in the given operand. (Set to one.)
    Set(u8, Operand8),

    /// `DAA`: Decimal adjust register A. This is used after adding or subtracting BCD
    /// (binary-coded decimal) values to obtain the correct result for BCD arithmetic.
    ///
    /// In BCD, register A contains 2 digits, one in the upper 4 bits, one in the lower 4 bits,
    /// each of which may have overflowed into non-BCD hex digits (i.e. greater than 9). For each
    /// digit, an adjustment value will be applied if necessary. If the Sub flag is set, the
    /// adjustment values are subtracted, otherwise added.
    ///
    /// DAA proceeds as follows:
    ///
    /// 1. If the lower 4 bits contain a non-BCD digit (i.e. greater than 9) or if the HalfCarry
    ///    flag is set, then 0x06 is added/subtracted in register A.
    /// 2. If the upper 4 bits contain a non-BCD digit (i.e. greater than 9) or if the Carry flag
    ///    is set, then 0x60 is added/subtracted in register A.
    Daa,

    /// `CPL`: Complement register A, i.e. flip all bits.
    Cpl,

    /// `CCF`: Complement the carry flag, i.e. invert its value.
    Ccf,

    /// `SCF`: Set the carry flag to true.
    Scf,

    /// An invalid opcode the CPU does not understand, Instruction decoding will generate these for
    /// bad opcodes and executing them will halt emulation and report an error. On the physical
    /// Game Boy, they supposedly cause it to "lock up".
    Invalid(u8),
}

impl Inst {
    /// Decode a Game Boy instruction from the given bytes.
    ///
    /// Panics if the slice is empty or if it isn't long enough for the instruction specified by its
    /// first byte (the opcode).
    pub fn from_bytes(bytes: &[u8]) -> Inst {
        use Inst::*;
        use Operand8::*;
        use Operand16::*;
        use crate::cpu::Reg16::*;
        use crate::cpu::Reg8::*;

        match *bytes {
            [0x00] => Nop,

            [0x01, low, high] => Ld16(Reg16(BC), Imm16(u16::from_le_bytes([low, high]))),
            [0x11, low, high] => Ld16(Reg16(DE), Imm16(u16::from_le_bytes([low, high]))),
            [0x21, low, high] => Ld16(Reg16(HL), Imm16(u16::from_le_bytes([low, high]))),
            [0x31, low, high] => Ld16(Reg16(SP), Imm16(u16::from_le_bytes([low, high]))),

            [0x08, low, high] => Ld16(MemImm16(u16::from_le_bytes([low, high])), Reg16(SP)),

            [0x02] => Ld8(MemReg(BC), Reg8(A)),
            [0x12] => Ld8(MemReg(DE), Reg8(A)),
            [0x0A] => Ld8(Reg8(A), MemReg(BC)),
            [0x1A] => Ld8(Reg8(A), MemReg(DE)),

            [0x03] => Inc16(Reg16(BC)),
            [0x13] => Inc16(Reg16(DE)),
            [0x23] => Inc16(Reg16(HL)),
            [0x33] => Inc16(Reg16(SP)),

            [0x0B] => Dec16(Reg16(BC)),
            [0x1B] => Dec16(Reg16(DE)),
            [0x2B] => Dec16(Reg16(HL)),
            [0x3B] => Dec16(Reg16(SP)),

            [0x04] => Inc8(Reg8(B)),
            [0x0C] => Inc8(Reg8(C)),
            [0x14] => Inc8(Reg8(D)),
            [0x1C] => Inc8(Reg8(E)),
            [0x24] => Inc8(Reg8(H)),
            [0x2C] => Inc8(Reg8(L)),
            [0x34] => Inc8(MemReg(HL)),
            [0x3C] => Inc8(Reg8(A)),

            [0x05] => Dec8(Reg8(B)),
            [0x0D] => Dec8(Reg8(C)),
            [0x15] => Dec8(Reg8(D)),
            [0x1D] => Dec8(Reg8(E)),
            [0x25] => Dec8(Reg8(H)),
            [0x2D] => Dec8(Reg8(L)),
            [0x35] => Dec8(MemReg(HL)),
            [0x3D] => Dec8(Reg8(A)),

            [0x06, val] => Ld8(Reg8(B), Imm8(val)),
            [0x0E, val] => Ld8(Reg8(C), Imm8(val)),
            [0x16, val] => Ld8(Reg8(D), Imm8(val)),
            [0x1E, val] => Ld8(Reg8(E), Imm8(val)),
            [0x26, val] => Ld8(Reg8(H), Imm8(val)),
            [0x2E, val] => Ld8(Reg8(L), Imm8(val)),
            [0x36, val] => Ld8(MemReg(HL), Imm8(val)),
            [0x3E, val] => Ld8(Reg8(A), Imm8(val)),

            [0x22] => Ld8(MemHlPostInc, Reg8(A)),
            [0x32] => Ld8(MemHlPostDec, Reg8(A)),
            [0x2A] => Ld8(Reg8(A), MemHlPostInc),
            [0x3A] => Ld8(Reg8(A), MemHlPostDec),

            [0xE0, addr] => Ld8(MemHighImm(addr), Reg8(A)),
            [0xF0, addr] => Ld8(Reg8(A), MemHighImm(addr)),

            [0xE2] => Ld8(MemHighC, Reg8(A)),
            [0xF2] => Ld8(Reg8(A), MemHighC),

            [0xEA, low, high] => Ld8(MemImm(u16::from_le_bytes([low, high])), Reg8(A)),
            [0xFA, low, high] => Ld8(Reg8(A), MemImm(u16::from_le_bytes([low, high]))),

            [0x07] => Rlca,
            [0x0F] => Rrca,
            [0x17] => Rla,
            [0x1F] => Rra,
            [0x27] => Daa,
            [0x2F] => Cpl,
            [0x37] => Scf,
            [0x3F] => Ccf,

            [0x09] => AddHl(Reg16(BC)),
            [0x19] => AddHl(Reg16(DE)),
            [0x29] => AddHl(Reg16(HL)),
            [0x39] => AddHl(Reg16(SP)),

            [0x10, zero] => {
                // FIXME(solson): A valid STOP instruction must be followed by 0x00 according to
                // the manual. I think I read that it has slightly different behaviour if it is
                // not zero. More investigation needed.
                if zero != 0 {
                    warn!("STOP's second byte was 0x{:02X} instead of 0x00", zero);
                }
                Stop
            }

            [0x76] => Halt,

            [0x18, offset] => Jr(offset as i8, Cond::None),
            [0x20, offset] => Jr(offset as i8, Cond::NotZero),
            [0x28, offset] => Jr(offset as i8, Cond::Zero),
            [0x30, offset] => Jr(offset as i8, Cond::NotCarry),
            [0x38, offset] => Jr(offset as i8, Cond::Carry),

            [0x40] => Ld8(Reg8(B), Reg8(B)),
            [0x41] => Ld8(Reg8(B), Reg8(C)),
            [0x42] => Ld8(Reg8(B), Reg8(D)),
            [0x43] => Ld8(Reg8(B), Reg8(E)),
            [0x44] => Ld8(Reg8(B), Reg8(H)),
            [0x45] => Ld8(Reg8(B), Reg8(L)),
            [0x46] => Ld8(Reg8(B), MemReg(HL)),
            [0x47] => Ld8(Reg8(B), Reg8(A)),

            [0x48] => Ld8(Reg8(C), Reg8(B)),
            [0x49] => Ld8(Reg8(C), Reg8(C)),
            [0x4A] => Ld8(Reg8(C), Reg8(D)),
            [0x4B] => Ld8(Reg8(C), Reg8(E)),
            [0x4C] => Ld8(Reg8(C), Reg8(H)),
            [0x4D] => Ld8(Reg8(C), Reg8(L)),
            [0x4E] => Ld8(Reg8(C), MemReg(HL)),
            [0x4F] => Ld8(Reg8(C), Reg8(A)),

            [0x50] => Ld8(Reg8(D), Reg8(B)),
            [0x51] => Ld8(Reg8(D), Reg8(C)),
            [0x52] => Ld8(Reg8(D), Reg8(D)),
            [0x53] => Ld8(Reg8(D), Reg8(E)),
            [0x54] => Ld8(Reg8(D), Reg8(H)),
            [0x55] => Ld8(Reg8(D), Reg8(L)),
            [0x56] => Ld8(Reg8(D), MemReg(HL)),
            [0x57] => Ld8(Reg8(D), Reg8(A)),

            [0x58] => Ld8(Reg8(E), Reg8(B)),
            [0x59] => Ld8(Reg8(E), Reg8(C)),
            [0x5A] => Ld8(Reg8(E), Reg8(D)),
            [0x5B] => Ld8(Reg8(E), Reg8(E)),
            [0x5C] => Ld8(Reg8(E), Reg8(H)),
            [0x5D] => Ld8(Reg8(E), Reg8(L)),
            [0x5E] => Ld8(Reg8(E), MemReg(HL)),
            [0x5F] => Ld8(Reg8(E), Reg8(A)),

            [0x60] => Ld8(Reg8(H), Reg8(B)),
            [0x61] => Ld8(Reg8(H), Reg8(C)),
            [0x62] => Ld8(Reg8(H), Reg8(D)),
            [0x63] => Ld8(Reg8(H), Reg8(E)),
            [0x64] => Ld8(Reg8(H), Reg8(H)),
            [0x65] => Ld8(Reg8(H), Reg8(L)),
            [0x66] => Ld8(Reg8(H), MemReg(HL)),
            [0x67] => Ld8(Reg8(H), Reg8(A)),

            [0x68] => Ld8(Reg8(L), Reg8(B)),
            [0x69] => Ld8(Reg8(L), Reg8(C)),
            [0x6A] => Ld8(Reg8(L), Reg8(D)),
            [0x6B] => Ld8(Reg8(L), Reg8(E)),
            [0x6C] => Ld8(Reg8(L), Reg8(H)),
            [0x6D] => Ld8(Reg8(L), Reg8(L)),
            [0x6E] => Ld8(Reg8(L), MemReg(HL)),
            [0x6F] => Ld8(Reg8(L), Reg8(A)),

            [0x70] => Ld8(MemReg(HL), Reg8(B)),
            [0x71] => Ld8(MemReg(HL), Reg8(C)),
            [0x72] => Ld8(MemReg(HL), Reg8(D)),
            [0x73] => Ld8(MemReg(HL), Reg8(E)),
            [0x74] => Ld8(MemReg(HL), Reg8(H)),
            [0x75] => Ld8(MemReg(HL), Reg8(L)),
            [0x77] => Ld8(MemReg(HL), Reg8(A)),

            [0x78] => Ld8(Reg8(A), Reg8(B)),
            [0x79] => Ld8(Reg8(A), Reg8(C)),
            [0x7A] => Ld8(Reg8(A), Reg8(D)),
            [0x7B] => Ld8(Reg8(A), Reg8(E)),
            [0x7C] => Ld8(Reg8(A), Reg8(H)),
            [0x7D] => Ld8(Reg8(A), Reg8(L)),
            [0x7E] => Ld8(Reg8(A), MemReg(HL)),
            [0x7F] => Ld8(Reg8(A), Reg8(A)),

            [0x80] => AddA(Reg8(B)),
            [0x81] => AddA(Reg8(C)),
            [0x82] => AddA(Reg8(D)),
            [0x83] => AddA(Reg8(E)),
            [0x84] => AddA(Reg8(H)),
            [0x85] => AddA(Reg8(L)),
            [0x86] => AddA(MemReg(HL)),
            [0x87] => AddA(Reg8(A)),

            [0x88] => AdcA(Reg8(B)),
            [0x89] => AdcA(Reg8(C)),
            [0x8A] => AdcA(Reg8(D)),
            [0x8B] => AdcA(Reg8(E)),
            [0x8C] => AdcA(Reg8(H)),
            [0x8D] => AdcA(Reg8(L)),
            [0x8E] => AdcA(MemReg(HL)),
            [0x8F] => AdcA(Reg8(A)),

            [0x90] => Sub(Reg8(B)),
            [0x91] => Sub(Reg8(C)),
            [0x92] => Sub(Reg8(D)),
            [0x93] => Sub(Reg8(E)),
            [0x94] => Sub(Reg8(H)),
            [0x95] => Sub(Reg8(L)),
            [0x96] => Sub(MemReg(HL)),
            [0x97] => Sub(Reg8(A)),

            [0x98] => SbcA(Reg8(B)),
            [0x99] => SbcA(Reg8(C)),
            [0x9A] => SbcA(Reg8(D)),
            [0x9B] => SbcA(Reg8(E)),
            [0x9C] => SbcA(Reg8(H)),
            [0x9D] => SbcA(Reg8(L)),
            [0x9E] => SbcA(MemReg(HL)),
            [0x9F] => SbcA(Reg8(A)),

            [0xA0] => And(Reg8(B)),
            [0xA1] => And(Reg8(C)),
            [0xA2] => And(Reg8(D)),
            [0xA3] => And(Reg8(E)),
            [0xA4] => And(Reg8(H)),
            [0xA5] => And(Reg8(L)),
            [0xA6] => And(MemReg(HL)),
            [0xA7] => And(Reg8(A)),

            [0xA8] => Xor(Reg8(B)),
            [0xA9] => Xor(Reg8(C)),
            [0xAA] => Xor(Reg8(D)),
            [0xAB] => Xor(Reg8(E)),
            [0xAC] => Xor(Reg8(H)),
            [0xAD] => Xor(Reg8(L)),
            [0xAE] => Xor(MemReg(HL)),
            [0xAF] => Xor(Reg8(A)),

            [0xB0] => Or(Reg8(B)),
            [0xB1] => Or(Reg8(C)),
            [0xB2] => Or(Reg8(D)),
            [0xB3] => Or(Reg8(E)),
            [0xB4] => Or(Reg8(H)),
            [0xB5] => Or(Reg8(L)),
            [0xB6] => Or(MemReg(HL)),
            [0xB7] => Or(Reg8(A)),

            [0xB8] => Cp(Reg8(B)),
            [0xB9] => Cp(Reg8(C)),
            [0xBA] => Cp(Reg8(D)),
            [0xBB] => Cp(Reg8(E)),
            [0xBC] => Cp(Reg8(H)),
            [0xBD] => Cp(Reg8(L)),
            [0xBE] => Cp(MemReg(HL)),
            [0xBF] => Cp(Reg8(A)),

            [0xC0] => Ret(Cond::NotZero),
            [0xC8] => Ret(Cond::Zero),
            [0xC9] => Ret(Cond::None),
            [0xD0] => Ret(Cond::NotCarry),
            [0xD8] => Ret(Cond::Carry),
            [0xD9] => Reti,

            [0xC1] => Pop(BC),
            [0xD1] => Pop(DE),
            [0xE1] => Pop(HL),
            [0xF1] => Pop(AF),

            [0xC5] => Push(BC),
            [0xD5] => Push(DE),
            [0xE5] => Push(HL),
            [0xF5] => Push(AF),

            [0xC2, low, high] => Jp(Imm16(u16::from_le_bytes([low, high])), Cond::NotZero),
            [0xC3, low, high] => Jp(Imm16(u16::from_le_bytes([low, high])), Cond::None),
            [0xCA, low, high] => Jp(Imm16(u16::from_le_bytes([low, high])), Cond::Zero),
            [0xD2, low, high] => Jp(Imm16(u16::from_le_bytes([low, high])), Cond::NotCarry),
            [0xDA, low, high] => Jp(Imm16(u16::from_le_bytes([low, high])), Cond::Carry),

            [0xC4, low, high] => Call(u16::from_le_bytes([low, high]), Cond::NotZero),
            [0xCC, low, high] => Call(u16::from_le_bytes([low, high]), Cond::Zero),
            [0xCD, low, high] => Call(u16::from_le_bytes([low, high]), Cond::None),
            [0xD4, low, high] => Call(u16::from_le_bytes([low, high]), Cond::NotCarry),
            [0xDC, low, high] => Call(u16::from_le_bytes([low, high]), Cond::Carry),

            [0xC6, val] => AddA(Imm8(val)),
            [0xCE, val] => AdcA(Imm8(val)),
            [0xD6, val] => Sub(Imm8(val)),
            [0xDE, val] => SbcA(Imm8(val)),
            [0xE6, val] => And(Imm8(val)),
            [0xEE, val] => Xor(Imm8(val)),
            [0xF6, val] => Or(Imm8(val)),
            [0xFE, val] => Cp(Imm8(val)),

            [0xC7] => Rst(0x00),
            [0xCF] => Rst(0x08),
            [0xD7] => Rst(0x10),
            [0xDF] => Rst(0x18),
            [0xE7] => Rst(0x20),
            [0xEF] => Rst(0x28),
            [0xF7] => Rst(0x30),
            [0xFF] => Rst(0x38),

            [b @ 0xD3] => Invalid(b),
            [b @ 0xDB] => Invalid(b),
            [b @ 0xDD] => Invalid(b),
            [b @ 0xE3] => Invalid(b),
            [b @ 0xE4] => Invalid(b),
            [b @ 0xEB] => Invalid(b),
            [b @ 0xEC] => Invalid(b),
            [b @ 0xED] => Invalid(b),
            [b @ 0xF4] => Invalid(b),
            [b @ 0xFC] => Invalid(b),
            [b @ 0xFD] => Invalid(b),

            [0xE8, offset] => AddSp(offset as i8),
            [0xF8, offset] => LdHlSp(offset as i8),

            [0xF3] => Di,
            [0xFB] => Ei,

            [0xE9] => Jp(Reg16(HL), Cond::None),
            [0xF9] => Ld16(Reg16(SP), Reg16(HL)),

            [0xCB, 0x00] => Rlc(Reg8(B)),
            [0xCB, 0x01] => Rlc(Reg8(C)),
            [0xCB, 0x02] => Rlc(Reg8(D)),
            [0xCB, 0x03] => Rlc(Reg8(E)),
            [0xCB, 0x04] => Rlc(Reg8(H)),
            [0xCB, 0x05] => Rlc(Reg8(L)),
            [0xCB, 0x06] => Rlc(MemReg(HL)),
            [0xCB, 0x07] => Rlc(Reg8(A)),

            [0xCB, 0x08] => Rrc(Reg8(B)),
            [0xCB, 0x09] => Rrc(Reg8(C)),
            [0xCB, 0x0A] => Rrc(Reg8(D)),
            [0xCB, 0x0B] => Rrc(Reg8(E)),
            [0xCB, 0x0C] => Rrc(Reg8(H)),
            [0xCB, 0x0D] => Rrc(Reg8(L)),
            [0xCB, 0x0E] => Rrc(MemReg(HL)),
            [0xCB, 0x0F] => Rrc(Reg8(A)),

            [0xCB, 0x10] => Rl(Reg8(B)),
            [0xCB, 0x11] => Rl(Reg8(C)),
            [0xCB, 0x12] => Rl(Reg8(D)),
            [0xCB, 0x13] => Rl(Reg8(E)),
            [0xCB, 0x14] => Rl(Reg8(H)),
            [0xCB, 0x15] => Rl(Reg8(L)),
            [0xCB, 0x16] => Rl(MemReg(HL)),
            [0xCB, 0x17] => Rl(Reg8(A)),

            [0xCB, 0x18] => Rr(Reg8(B)),
            [0xCB, 0x19] => Rr(Reg8(C)),
            [0xCB, 0x1A] => Rr(Reg8(D)),
            [0xCB, 0x1B] => Rr(Reg8(E)),
            [0xCB, 0x1C] => Rr(Reg8(H)),
            [0xCB, 0x1D] => Rr(Reg8(L)),
            [0xCB, 0x1E] => Rr(MemReg(HL)),
            [0xCB, 0x1F] => Rr(Reg8(A)),

            [0xCB, 0x20] => Sla(Reg8(B)),
            [0xCB, 0x21] => Sla(Reg8(C)),
            [0xCB, 0x22] => Sla(Reg8(D)),
            [0xCB, 0x23] => Sla(Reg8(E)),
            [0xCB, 0x24] => Sla(Reg8(H)),
            [0xCB, 0x25] => Sla(Reg8(L)),
            [0xCB, 0x26] => Sla(MemReg(HL)),
            [0xCB, 0x27] => Sla(Reg8(A)),

            [0xCB, 0x28] => Sra(Reg8(B)),
            [0xCB, 0x29] => Sra(Reg8(C)),
            [0xCB, 0x2A] => Sra(Reg8(D)),
            [0xCB, 0x2B] => Sra(Reg8(E)),
            [0xCB, 0x2C] => Sra(Reg8(H)),
            [0xCB, 0x2D] => Sra(Reg8(L)),
            [0xCB, 0x2E] => Sra(MemReg(HL)),
            [0xCB, 0x2F] => Sra(Reg8(A)),

            [0xCB, 0x30] => Swap(Reg8(B)),
            [0xCB, 0x31] => Swap(Reg8(C)),
            [0xCB, 0x32] => Swap(Reg8(D)),
            [0xCB, 0x33] => Swap(Reg8(E)),
            [0xCB, 0x34] => Swap(Reg8(H)),
            [0xCB, 0x35] => Swap(Reg8(L)),
            [0xCB, 0x36] => Swap(MemReg(HL)),
            [0xCB, 0x37] => Swap(Reg8(A)),

            [0xCB, 0x38] => Srl(Reg8(B)),
            [0xCB, 0x39] => Srl(Reg8(C)),
            [0xCB, 0x3A] => Srl(Reg8(D)),
            [0xCB, 0x3B] => Srl(Reg8(E)),
            [0xCB, 0x3C] => Srl(Reg8(H)),
            [0xCB, 0x3D] => Srl(Reg8(L)),
            [0xCB, 0x3E] => Srl(MemReg(HL)),
            [0xCB, 0x3F] => Srl(Reg8(A)),

            [0xCB, 0x40] => Bit(0, Reg8(B)),
            [0xCB, 0x41] => Bit(0, Reg8(C)),
            [0xCB, 0x42] => Bit(0, Reg8(D)),
            [0xCB, 0x43] => Bit(0, Reg8(E)),
            [0xCB, 0x44] => Bit(0, Reg8(H)),
            [0xCB, 0x45] => Bit(0, Reg8(L)),
            [0xCB, 0x46] => Bit(0, MemReg(HL)),
            [0xCB, 0x47] => Bit(0, Reg8(A)),

            [0xCB, 0x48] => Bit(1, Reg8(B)),
            [0xCB, 0x49] => Bit(1, Reg8(C)),
            [0xCB, 0x4A] => Bit(1, Reg8(D)),
            [0xCB, 0x4B] => Bit(1, Reg8(E)),
            [0xCB, 0x4C] => Bit(1, Reg8(H)),
            [0xCB, 0x4D] => Bit(1, Reg8(L)),
            [0xCB, 0x4E] => Bit(1, MemReg(HL)),
            [0xCB, 0x4F] => Bit(1, Reg8(A)),

            [0xCB, 0x50] => Bit(2, Reg8(B)),
            [0xCB, 0x51] => Bit(2, Reg8(C)),
            [0xCB, 0x52] => Bit(2, Reg8(D)),
            [0xCB, 0x53] => Bit(2, Reg8(E)),
            [0xCB, 0x54] => Bit(2, Reg8(H)),
            [0xCB, 0x55] => Bit(2, Reg8(L)),
            [0xCB, 0x56] => Bit(2, MemReg(HL)),
            [0xCB, 0x57] => Bit(2, Reg8(A)),

            [0xCB, 0x58] => Bit(3, Reg8(B)),
            [0xCB, 0x59] => Bit(3, Reg8(C)),
            [0xCB, 0x5A] => Bit(3, Reg8(D)),
            [0xCB, 0x5B] => Bit(3, Reg8(E)),
            [0xCB, 0x5C] => Bit(3, Reg8(H)),
            [0xCB, 0x5D] => Bit(3, Reg8(L)),
            [0xCB, 0x5E] => Bit(3, MemReg(HL)),
            [0xCB, 0x5F] => Bit(3, Reg8(A)),

            [0xCB, 0x60] => Bit(4, Reg8(B)),
            [0xCB, 0x61] => Bit(4, Reg8(C)),
            [0xCB, 0x62] => Bit(4, Reg8(D)),
            [0xCB, 0x63] => Bit(4, Reg8(E)),
            [0xCB, 0x64] => Bit(4, Reg8(H)),
            [0xCB, 0x65] => Bit(4, Reg8(L)),
            [0xCB, 0x66] => Bit(4, MemReg(HL)),
            [0xCB, 0x67] => Bit(4, Reg8(A)),

            [0xCB, 0x68] => Bit(5, Reg8(B)),
            [0xCB, 0x69] => Bit(5, Reg8(C)),
            [0xCB, 0x6A] => Bit(5, Reg8(D)),
            [0xCB, 0x6B] => Bit(5, Reg8(E)),
            [0xCB, 0x6C] => Bit(5, Reg8(H)),
            [0xCB, 0x6D] => Bit(5, Reg8(L)),
            [0xCB, 0x6E] => Bit(5, MemReg(HL)),
            [0xCB, 0x6F] => Bit(5, Reg8(A)),

            [0xCB, 0x70] => Bit(6, Reg8(B)),
            [0xCB, 0x71] => Bit(6, Reg8(C)),
            [0xCB, 0x72] => Bit(6, Reg8(D)),
            [0xCB, 0x73] => Bit(6, Reg8(E)),
            [0xCB, 0x74] => Bit(6, Reg8(H)),
            [0xCB, 0x75] => Bit(6, Reg8(L)),
            [0xCB, 0x76] => Bit(6, MemReg(HL)),
            [0xCB, 0x77] => Bit(6, Reg8(A)),

            [0xCB, 0x78] => Bit(7, Reg8(B)),
            [0xCB, 0x79] => Bit(7, Reg8(C)),
            [0xCB, 0x7A] => Bit(7, Reg8(D)),
            [0xCB, 0x7B] => Bit(7, Reg8(E)),
            [0xCB, 0x7C] => Bit(7, Reg8(H)),
            [0xCB, 0x7D] => Bit(7, Reg8(L)),
            [0xCB, 0x7E] => Bit(7, MemReg(HL)),
            [0xCB, 0x7F] => Bit(7, Reg8(A)),

            [0xCB, 0x80] => Res(0, Reg8(B)),
            [0xCB, 0x81] => Res(0, Reg8(C)),
            [0xCB, 0x82] => Res(0, Reg8(D)),
            [0xCB, 0x83] => Res(0, Reg8(E)),
            [0xCB, 0x84] => Res(0, Reg8(H)),
            [0xCB, 0x85] => Res(0, Reg8(L)),
            [0xCB, 0x86] => Res(0, MemReg(HL)),
            [0xCB, 0x87] => Res(0, Reg8(A)),

            [0xCB, 0x88] => Res(1, Reg8(B)),
            [0xCB, 0x89] => Res(1, Reg8(C)),
            [0xCB, 0x8A] => Res(1, Reg8(D)),
            [0xCB, 0x8B] => Res(1, Reg8(E)),
            [0xCB, 0x8C] => Res(1, Reg8(H)),
            [0xCB, 0x8D] => Res(1, Reg8(L)),
            [0xCB, 0x8E] => Res(1, MemReg(HL)),
            [0xCB, 0x8F] => Res(1, Reg8(A)),

            [0xCB, 0x90] => Res(2, Reg8(B)),
            [0xCB, 0x91] => Res(2, Reg8(C)),
            [0xCB, 0x92] => Res(2, Reg8(D)),
            [0xCB, 0x93] => Res(2, Reg8(E)),
            [0xCB, 0x94] => Res(2, Reg8(H)),
            [0xCB, 0x95] => Res(2, Reg8(L)),
            [0xCB, 0x96] => Res(2, MemReg(HL)),
            [0xCB, 0x97] => Res(2, Reg8(A)),

            [0xCB, 0x98] => Res(3, Reg8(B)),
            [0xCB, 0x99] => Res(3, Reg8(C)),
            [0xCB, 0x9A] => Res(3, Reg8(D)),
            [0xCB, 0x9B] => Res(3, Reg8(E)),
            [0xCB, 0x9C] => Res(3, Reg8(H)),
            [0xCB, 0x9D] => Res(3, Reg8(L)),
            [0xCB, 0x9E] => Res(3, MemReg(HL)),
            [0xCB, 0x9F] => Res(3, Reg8(A)),

            [0xCB, 0xA0] => Res(4, Reg8(B)),
            [0xCB, 0xA1] => Res(4, Reg8(C)),
            [0xCB, 0xA2] => Res(4, Reg8(D)),
            [0xCB, 0xA3] => Res(4, Reg8(E)),
            [0xCB, 0xA4] => Res(4, Reg8(H)),
            [0xCB, 0xA5] => Res(4, Reg8(L)),
            [0xCB, 0xA6] => Res(4, MemReg(HL)),
            [0xCB, 0xA7] => Res(4, Reg8(A)),

            [0xCB, 0xA8] => Res(5, Reg8(B)),
            [0xCB, 0xA9] => Res(5, Reg8(C)),
            [0xCB, 0xAA] => Res(5, Reg8(D)),
            [0xCB, 0xAB] => Res(5, Reg8(E)),
            [0xCB, 0xAC] => Res(5, Reg8(H)),
            [0xCB, 0xAD] => Res(5, Reg8(L)),
            [0xCB, 0xAE] => Res(5, MemReg(HL)),
            [0xCB, 0xAF] => Res(5, Reg8(A)),

            [0xCB, 0xB0] => Res(6, Reg8(B)),
            [0xCB, 0xB1] => Res(6, Reg8(C)),
            [0xCB, 0xB2] => Res(6, Reg8(D)),
            [0xCB, 0xB3] => Res(6, Reg8(E)),
            [0xCB, 0xB4] => Res(6, Reg8(H)),
            [0xCB, 0xB5] => Res(6, Reg8(L)),
            [0xCB, 0xB6] => Res(6, MemReg(HL)),
            [0xCB, 0xB7] => Res(6, Reg8(A)),

            [0xCB, 0xB8] => Res(7, Reg8(B)),
            [0xCB, 0xB9] => Res(7, Reg8(C)),
            [0xCB, 0xBA] => Res(7, Reg8(D)),
            [0xCB, 0xBB] => Res(7, Reg8(E)),
            [0xCB, 0xBC] => Res(7, Reg8(H)),
            [0xCB, 0xBD] => Res(7, Reg8(L)),
            [0xCB, 0xBE] => Res(7, MemReg(HL)),
            [0xCB, 0xBF] => Res(7, Reg8(A)),

            [0xCB, 0xC0] => Set(0, Reg8(B)),
            [0xCB, 0xC1] => Set(0, Reg8(C)),
            [0xCB, 0xC2] => Set(0, Reg8(D)),
            [0xCB, 0xC3] => Set(0, Reg8(E)),
            [0xCB, 0xC4] => Set(0, Reg8(H)),
            [0xCB, 0xC5] => Set(0, Reg8(L)),
            [0xCB, 0xC6] => Set(0, MemReg(HL)),
            [0xCB, 0xC7] => Set(0, Reg8(A)),

            [0xCB, 0xC8] => Set(1, Reg8(B)),
            [0xCB, 0xC9] => Set(1, Reg8(C)),
            [0xCB, 0xCA] => Set(1, Reg8(D)),
            [0xCB, 0xCB] => Set(1, Reg8(E)),
            [0xCB, 0xCC] => Set(1, Reg8(H)),
            [0xCB, 0xCD] => Set(1, Reg8(L)),
            [0xCB, 0xCE] => Set(1, MemReg(HL)),
            [0xCB, 0xCF] => Set(1, Reg8(A)),

            [0xCB, 0xD0] => Set(2, Reg8(B)),
            [0xCB, 0xD1] => Set(2, Reg8(C)),
            [0xCB, 0xD2] => Set(2, Reg8(D)),
            [0xCB, 0xD3] => Set(2, Reg8(E)),
            [0xCB, 0xD4] => Set(2, Reg8(H)),
            [0xCB, 0xD5] => Set(2, Reg8(L)),
            [0xCB, 0xD6] => Set(2, MemReg(HL)),
            [0xCB, 0xD7] => Set(2, Reg8(A)),

            [0xCB, 0xD8] => Set(3, Reg8(B)),
            [0xCB, 0xD9] => Set(3, Reg8(C)),
            [0xCB, 0xDA] => Set(3, Reg8(D)),
            [0xCB, 0xDB] => Set(3, Reg8(E)),
            [0xCB, 0xDC] => Set(3, Reg8(H)),
            [0xCB, 0xDD] => Set(3, Reg8(L)),
            [0xCB, 0xDE] => Set(3, MemReg(HL)),
            [0xCB, 0xDF] => Set(3, Reg8(A)),

            [0xCB, 0xE0] => Set(4, Reg8(B)),
            [0xCB, 0xE1] => Set(4, Reg8(C)),
            [0xCB, 0xE2] => Set(4, Reg8(D)),
            [0xCB, 0xE3] => Set(4, Reg8(E)),
            [0xCB, 0xE4] => Set(4, Reg8(H)),
            [0xCB, 0xE5] => Set(4, Reg8(L)),
            [0xCB, 0xE6] => Set(4, MemReg(HL)),
            [0xCB, 0xE7] => Set(4, Reg8(A)),

            [0xCB, 0xE8] => Set(5, Reg8(B)),
            [0xCB, 0xE9] => Set(5, Reg8(C)),
            [0xCB, 0xEA] => Set(5, Reg8(D)),
            [0xCB, 0xEB] => Set(5, Reg8(E)),
            [0xCB, 0xEC] => Set(5, Reg8(H)),
            [0xCB, 0xED] => Set(5, Reg8(L)),
            [0xCB, 0xEE] => Set(5, MemReg(HL)),
            [0xCB, 0xEF] => Set(5, Reg8(A)),

            [0xCB, 0xF0] => Set(6, Reg8(B)),
            [0xCB, 0xF1] => Set(6, Reg8(C)),
            [0xCB, 0xF2] => Set(6, Reg8(D)),
            [0xCB, 0xF3] => Set(6, Reg8(E)),
            [0xCB, 0xF4] => Set(6, Reg8(H)),
            [0xCB, 0xF5] => Set(6, Reg8(L)),
            [0xCB, 0xF6] => Set(6, MemReg(HL)),
            [0xCB, 0xF7] => Set(6, Reg8(A)),

            [0xCB, 0xF8] => Set(7, Reg8(B)),
            [0xCB, 0xF9] => Set(7, Reg8(C)),
            [0xCB, 0xFA] => Set(7, Reg8(D)),
            [0xCB, 0xFB] => Set(7, Reg8(E)),
            [0xCB, 0xFC] => Set(7, Reg8(H)),
            [0xCB, 0xFD] => Set(7, Reg8(L)),
            [0xCB, 0xFE] => Set(7, MemReg(HL)),
            [0xCB, 0xFF] => Set(7, Reg8(A)),

            _ => unreachable!(),
        }
    }
}
