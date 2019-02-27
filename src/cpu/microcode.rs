use crate::cpu::inst::Cond;
use crate::cpu::registers::{Reg16, Reg8};
use crate::cpu::registers::Reg16::*;
use crate::cpu::registers::Reg8::*;
use self::Local::*;
use self::MicroInst::*;

#[derive(Clone, Copy, Debug)]
pub enum Local {
    Reg(Reg8),
    RegLow(Reg16),
    RegHigh(Reg16),
}

#[derive(Clone, Copy, Debug)]
pub enum Mem {
    Reg(Reg16),
    High(Local),
}

#[derive(Clone, Copy, Debug)]
pub enum MicroInst {
    Read(Local, Mem),
    Write(Mem, Local),

    Move16(Reg16, Reg16),
    MoveConst16(Reg16, u16),
    Inc16(Reg16),
    Dec16(Reg16),
    AddHl(Reg16),

    /// Represents `reg16 = reg16 + reg8` where the `reg8` is interpreted as a signed 2's
    /// complement number.
    AddOffset {
        dest: Reg16,
        src: Reg16,
        offset: Local,
        set_flags: bool,
    },

    Move8(Reg8, Reg8),
    Inc8(Local),
    Dec8(Local),

    Add(Local),
    Adc(Local),
    Sub(Local),
    Sbc(Local),
    And(Local),
    Xor(Local),
    Or(Local),
    Cp(Local),

    Rlca,
    Rrca,
    Rla,
    Rra,
    Daa,
    Cpl,
    Scf,
    Ccf,

    Rlc(Local),
    Rrc(Local),
    Rl(Local),
    Rr(Local),
    Sla(Local),
    Sra(Local),
    Swap(Local),
    Srl(Local),

    Bit(u8, Local),
    Res(u8, Local),
    Set(u8, Local),

    Halt,
    EnableInterrupts,
    EnableInterruptsDelayed,
    DisableInterruptsDelayed,
    DecodePrefixed,
    CheckCond(Cond),
    Unimplemented,
}

// `LD reg16, imm16`
macro_rules! load16_imm {
    ($dest:expr) => (&[
        &[Inc16(PC)],
        &[Read(RegLow($dest), Mem::Reg(PC)), Inc16(PC)],
        &[Read(RegHigh($dest), Mem::Reg(PC)), Inc16(PC)],
    ]);
}

// `INC reg16`
macro_rules! inc16 {
    ($reg:expr) => (&[
        &[Inc16(PC)],
        &[Inc16($reg)],
    ]);
}

// `DEC reg16`
macro_rules! dec16 {
    ($reg:expr) => (&[
        &[Inc16(PC)],
        &[Dec16($reg)],
    ]);
}

// `ADD HL, reg16`
macro_rules! add_hl {
    ($rhs:expr) => (&[
        &[Inc16(PC)],
        &[AddHl($rhs)],
    ]);
}

// `LD reg8, reg8`
macro_rules! load8 {
    ($dest:expr, $src:expr) => (&[
        &[Inc16(PC), Move8($dest, $src)],
    ]);
}

// `LD reg8, imm8`
macro_rules! load8_imm {
    ($dest:expr) => (&[
        &[Inc16(PC)],
        &[Read(Reg($dest), Mem::Reg(PC)), Inc16(PC)],
    ]);
}

// `LD reg8, (reg16)`
macro_rules! load8_from_memreg {
    ($dest:expr, $src:expr) => (&[
        &[Inc16(PC)],
        &[Read(Reg($dest), Mem::Reg($src))],
    ]);
}

// `LD (reg16), reg8`
macro_rules! load8_to_memreg {
    ($dest:expr, $src:expr) => (&[
        &[Inc16(PC)],
        &[Write(Mem::Reg($dest), Reg($src))],
    ]);
}

// `INC reg8`
macro_rules! inc8 {
    ($reg:expr) => (&[
        &[Inc16(PC), Inc8(Reg($reg))],
    ]);
}

// `DEC reg8`
macro_rules! dec8 {
    ($reg:expr) => (&[
        &[Inc16(PC), Dec8(Reg($reg))],
    ]);
}

// `{ADD,SUB} A, reg8`
// `{ADC,SBC,AND,XOR,OR,CP} reg8`
macro_rules! accum_op {
    ($op:ident, $reg:expr) => (&[
        &[Inc16(PC), $op(Reg($reg))],
    ]);
}

// `{ADD,SUB} A, imm8`
// `{ADC,SBC,AND,XOR,OR,CP} imm8`
macro_rules! accum_op_imm {
    ($op:ident) => (&[
        &[Inc16(PC)],
        &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC), $op(RegLow(Temp))],
    ]);
}

// `{ADD,SUB} A, (HL)`
// `{ADC,SBC,AND,XOR,OR,CP} (HL)`
macro_rules! accum_op_hl {
    ($op:ident) => (&[
        &[Inc16(PC)],
        &[Read(RegLow(Temp), Mem::Reg(HL)), $op(RegLow(Temp))],
    ]);
}

// `PUSH reg16`
macro_rules! push {
    ($src:expr) => (&[
        &[Inc16(PC)],
        &[Dec16(SP)],
        &[Write(Mem::Reg(SP), RegHigh($src)), Dec16(SP)],
        &[Write(Mem::Reg(SP), RegLow($src))],
    ]);
}

// `POP reg16`
macro_rules! pop {
    ($dest:expr) => (&[
        &[Inc16(PC)],
        &[Read(RegLow($dest), Mem::Reg(SP)), Inc16(SP)],
        &[Read(RegHigh($dest), Mem::Reg(SP)), Inc16(SP)],
    ]);
}

// `JP cond, imm16`
// `JP imm16` (using Cond::None)
macro_rules! jump {
    ($cond:expr) => (&[
        &[Inc16(PC)],
        &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
        &[Read(RegHigh(Temp), Mem::Reg(PC)), Inc16(PC), CheckCond($cond)],
        &[Move16(PC, Temp)],
    ]);
}

// `JR cond, signed_imm8`
// `JR signed_imm8` (using Cond::None)
macro_rules! jump_relative {
    ($cond:expr) => (&[
        &[Inc16(PC)],
        &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC), CheckCond($cond)],
        &[AddOffset { dest: PC, src: PC, offset: RegLow(Temp), set_flags: false }],
    ]);
}

// `CALL cond, imm16`
// `CALL imm16` (using Cond::None)
macro_rules! call {
    ($cond:expr) => (&[
        &[Inc16(PC)],
        &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
        &[Read(RegHigh(Temp), Mem::Reg(PC)), Inc16(PC), CheckCond($cond)],
        &[Dec16(SP)],
        &[Write(Mem::Reg(SP), RegHigh(PC)), Dec16(SP)],
        &[Write(Mem::Reg(SP), RegLow(PC)), Move16(PC, Temp)],
    ]);
}

// `RET cond`
macro_rules! ret_cond {
    ($cond:expr) => (&[
        &[Inc16(PC)],
        &[CheckCond($cond)],
        &[Read(RegLow(Temp), Mem::Reg(SP)), Inc16(SP)],
        &[Read(RegHigh(Temp), Mem::Reg(SP)), Inc16(SP)],
        &[Move16(PC, Temp)],
    ]);
}

// `RST 0x{00,08,10,18,20,28,30,38}`
macro_rules! rst {
    ($addr:expr) => (&[
        &[Inc16(PC)],
        &[Dec16(SP)],
        &[Write(Mem::Reg(SP), RegHigh(PC)), Dec16(SP)],
        &[Write(Mem::Reg(SP), RegLow(PC)), MoveConst16(PC, $addr)],
    ]);
}

pub fn microcode(opcode: u8) -> &'static [&'static [MicroInst]] {
    match opcode {
        // `NOP`
        0x00 => &[&[Inc16(PC)]],

        // `LD reg16, imm16`
        0x01 => load16_imm!(BC),
        0x11 => load16_imm!(DE),
        0x21 => load16_imm!(HL),
        0x31 => load16_imm!(SP),

        // LD SP, HL
        0xF9 => &[
            &[Inc16(PC)],
            &[Move16(SP, HL)],
        ],

        // `LD (imm16), SP`
        0x08 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Read(RegHigh(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Write(Mem::Reg(Temp), RegLow(SP)), Inc16(Temp)],
            &[Write(Mem::Reg(Temp), RegHigh(SP))],
        ],

        // `LD HL, SP+signed_imm8`
        0xF8 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[AddOffset { dest: HL, src: SP, offset: RegLow(Temp), set_flags: true }],
        ],

        // `ADD SP. signed_imm8`
        0xE8 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[AddOffset { dest: SP, src: SP, offset: RegLow(Temp), set_flags: true }],
            // TODO(solson): I have no good hypothesis why this takes one more cycle than
            // `LD HL, SP+signed_imm8`. For now, we just idle for an extra cycle.
            &[],
        ],

        // `INC reg16`
        0x03 => inc16!(BC),
        0x13 => inc16!(DE),
        0x23 => inc16!(HL),
        0x33 => inc16!(SP),

        // `DEC reg16`
        0x0B => dec16!(BC),
        0x1B => dec16!(DE),
        0x2B => dec16!(HL),
        0x3B => dec16!(SP),

        // `ADD HL, reg16`
        0x09 => add_hl!(BC),
        0x19 => add_hl!(DE),
        0x29 => add_hl!(HL),
        0x39 => add_hl!(SP),

        // `LD reg8, reg8`
        0x40 => load8!(B, B),
        0x41 => load8!(B, C),
        0x42 => load8!(B, D),
        0x43 => load8!(B, E),
        0x44 => load8!(B, H),
        0x45 => load8!(B, L),
        0x47 => load8!(B, A),
        0x48 => load8!(C, B),
        0x49 => load8!(C, C),
        0x4A => load8!(C, D),
        0x4B => load8!(C, E),
        0x4C => load8!(C, H),
        0x4D => load8!(C, L),
        0x4F => load8!(C, A),
        0x50 => load8!(D, B),
        0x51 => load8!(D, C),
        0x52 => load8!(D, D),
        0x53 => load8!(D, E),
        0x54 => load8!(D, H),
        0x55 => load8!(D, L),
        0x57 => load8!(D, A),
        0x58 => load8!(E, B),
        0x59 => load8!(E, C),
        0x5A => load8!(E, D),
        0x5B => load8!(E, E),
        0x5C => load8!(E, H),
        0x5D => load8!(E, L),
        0x5F => load8!(E, A),
        0x60 => load8!(H, B),
        0x61 => load8!(H, C),
        0x62 => load8!(H, D),
        0x63 => load8!(H, E),
        0x64 => load8!(H, H),
        0x65 => load8!(H, L),
        0x67 => load8!(H, A),
        0x68 => load8!(L, B),
        0x69 => load8!(L, C),
        0x6A => load8!(L, D),
        0x6B => load8!(L, E),
        0x6C => load8!(L, H),
        0x6D => load8!(L, L),
        0x6F => load8!(L, A),
        0x78 => load8!(A, B),
        0x79 => load8!(A, C),
        0x7A => load8!(A, D),
        0x7B => load8!(A, E),
        0x7C => load8!(A, H),
        0x7D => load8!(A, L),
        0x7F => load8!(A, A),

        // `LD reg8, imm8`
        0x06 => load8_imm!(B),
        0x0E => load8_imm!(C),
        0x16 => load8_imm!(D),
        0x1E => load8_imm!(E),
        0x26 => load8_imm!(H),
        0x2E => load8_imm!(L),
        0x3E => load8_imm!(A),

        // `LD reg8, (reg16)`
        0x0A => load8_from_memreg!(A, BC),
        0x1A => load8_from_memreg!(A, DE),
        0x46 => load8_from_memreg!(B, HL),
        0x4E => load8_from_memreg!(C, HL),
        0x56 => load8_from_memreg!(D, HL),
        0x5E => load8_from_memreg!(E, HL),
        0x66 => load8_from_memreg!(H, HL),
        0x6E => load8_from_memreg!(L, HL),
        0x7E => load8_from_memreg!(A, HL),

        // `LD (reg16), reg8`
        0x02 => load8_to_memreg!(BC, A),
        0x12 => load8_to_memreg!(DE, A),
        0x70 => load8_to_memreg!(HL, B),
        0x71 => load8_to_memreg!(HL, C),
        0x72 => load8_to_memreg!(HL, D),
        0x73 => load8_to_memreg!(HL, E),
        0x74 => load8_to_memreg!(HL, H),
        0x75 => load8_to_memreg!(HL, L),
        0x77 => load8_to_memreg!(HL, A),

        // `LD (HL), imm8`
        0x36 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Write(Mem::Reg(HL), RegLow(Temp))],
        ],

        // `LD (HL+), A`
        0x22 => &[
            &[Inc16(PC)],
            &[Write(Mem::Reg(HL), Reg(A)), Inc16(HL)],
        ],

        // `LD (HL-), A`
        0x32 => &[
            &[Inc16(PC)],
            &[Write(Mem::Reg(HL), Reg(A)), Dec16(HL)],
        ],

        // `LD A, (HL+)`
        0x2A => &[
            &[Inc16(PC)],
            &[Read(Reg(A), Mem::Reg(HL)), Inc16(HL)],
        ],

        // `LD A, (HL-)`
        0x3A => &[
            &[Inc16(PC)],
            &[Read(Reg(A), Mem::Reg(HL)), Dec16(HL)],
        ],

        // `LD (imm16), A`
        0xEA => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Read(RegHigh(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Write(Mem::Reg(Temp), Reg(A))],
        ],

        // `LD A, (imm16)`
        0xFA => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Read(RegHigh(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Read(Reg(A), Mem::Reg(Temp))],
        ],

        // `LD (0xFF00+imm8), A`
        0xE0 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Write(Mem::High(RegLow(Temp)), Reg(A))],
        ],

        // `LD A, (0xFF00+imm8)`
        0xF0 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(PC)), Inc16(PC)],
            &[Read(Reg(A), Mem::High(RegLow(Temp)))],
        ],

        // `LD (0xFF00+C), A`
        0xE2 => &[
            &[Inc16(PC)],
            &[Write(Mem::High(Reg(C)), Reg(A))],
        ],

        // `LD A, (0xFF00+imm8)`
        0xF2 => &[
            &[Inc16(PC)],
            &[Read(Reg(A), Mem::High(Reg(C)))],
        ],

        // TODO(solson): Merge with `accum_op`?
        // `INC reg8`
        0x04 => inc8!(B),
        0x0C => inc8!(C),
        0x14 => inc8!(D),
        0x1C => inc8!(E),
        0x24 => inc8!(H),
        0x2C => inc8!(L),
        0x3C => inc8!(A),

        // `DEC reg8`
        0x05 => dec8!(B),
        0x0D => dec8!(C),
        0x15 => dec8!(D),
        0x1D => dec8!(E),
        0x25 => dec8!(H),
        0x2D => dec8!(L),
        0x3D => dec8!(A),

        // TODO(solson): Merge with `accum_op_hl`?
        // `INC (HL)`
        0x34 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(HL)), Inc8(RegLow(Temp))],
            &[Write(Mem::Reg(HL), RegLow(Temp))],
        ],

        // `DEC (HL)`
        0x35 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(HL)), Dec8(RegLow(Temp))],
            &[Write(Mem::Reg(HL), RegLow(Temp))],
        ],

        // TODO(solson): Inst names inside `` are wrong here.
        // `{ADD,SUB} A, reg8`
        // `{ADC,SBC,AND,XOR,OR,CP} reg8`
        0x80 => accum_op!(Add, B),
        0x81 => accum_op!(Add, C),
        0x82 => accum_op!(Add, D),
        0x83 => accum_op!(Add, E),
        0x84 => accum_op!(Add, H),
        0x85 => accum_op!(Add, L),
        0x87 => accum_op!(Add, A),
        0x88 => accum_op!(Adc, B),
        0x89 => accum_op!(Adc, C),
        0x8A => accum_op!(Adc, D),
        0x8B => accum_op!(Adc, E),
        0x8C => accum_op!(Adc, H),
        0x8D => accum_op!(Adc, L),
        0x8F => accum_op!(Adc, A),
        0x90 => accum_op!(Sub, B),
        0x91 => accum_op!(Sub, C),
        0x92 => accum_op!(Sub, D),
        0x93 => accum_op!(Sub, E),
        0x94 => accum_op!(Sub, H),
        0x95 => accum_op!(Sub, L),
        0x97 => accum_op!(Sub, A),
        0x98 => accum_op!(Sbc, B),
        0x99 => accum_op!(Sbc, C),
        0x9A => accum_op!(Sbc, D),
        0x9B => accum_op!(Sbc, E),
        0x9C => accum_op!(Sbc, H),
        0x9D => accum_op!(Sbc, L),
        0x9F => accum_op!(Sbc, A),
        0xA0 => accum_op!(And, B),
        0xA1 => accum_op!(And, C),
        0xA2 => accum_op!(And, D),
        0xA3 => accum_op!(And, E),
        0xA4 => accum_op!(And, H),
        0xA5 => accum_op!(And, L),
        0xA7 => accum_op!(And, A),
        0xA8 => accum_op!(Xor, B),
        0xA9 => accum_op!(Xor, C),
        0xAA => accum_op!(Xor, D),
        0xAB => accum_op!(Xor, E),
        0xAC => accum_op!(Xor, H),
        0xAD => accum_op!(Xor, L),
        0xAF => accum_op!(Xor, A),
        0xB0 => accum_op!(Or, B),
        0xB1 => accum_op!(Or, C),
        0xB2 => accum_op!(Or, D),
        0xB3 => accum_op!(Or, E),
        0xB4 => accum_op!(Or, H),
        0xB5 => accum_op!(Or, L),
        0xB7 => accum_op!(Or, A),
        0xB8 => accum_op!(Cp, B),
        0xB9 => accum_op!(Cp, C),
        0xBA => accum_op!(Cp, D),
        0xBB => accum_op!(Cp, E),
        0xBC => accum_op!(Cp, H),
        0xBD => accum_op!(Cp, L),
        0xBF => accum_op!(Cp, A),

        // `{ADD,SUB} A, imm8`
        // `{ADC,SBC,AND,XOR,OR,CP} imm8`
        0xC6 => accum_op_imm!(Add),
        0xCE => accum_op_imm!(Adc),
        0xD6 => accum_op_imm!(Sub),
        0xDE => accum_op_imm!(Sbc),
        0xE6 => accum_op_imm!(And),
        0xEE => accum_op_imm!(Xor),
        0xF6 => accum_op_imm!(Or),
        0xFE => accum_op_imm!(Cp),

        // `{ADD,SUB} A, (HL)`
        // `{ADC,SBC,AND,XOR,OR,CP} (HL)`
        0x86 => accum_op_hl!(Add),
        0x8E => accum_op_hl!(Adc),
        0x96 => accum_op_hl!(Sub),
        0x9E => accum_op_hl!(Sbc),
        0xA6 => accum_op_hl!(And),
        0xAE => accum_op_hl!(Xor),
        0xB6 => accum_op_hl!(Or),
        0xBE => accum_op_hl!(Cp),

        0x07 => &[&[Inc16(PC), Rlca]],
        0x0F => &[&[Inc16(PC), Rrca]],
        0x17 => &[&[Inc16(PC), Rla]],
        0x1F => &[&[Inc16(PC), Rra]],
        0x27 => &[&[Inc16(PC), Daa]],
        0x2F => &[&[Inc16(PC), Cpl]],
        0x37 => &[&[Inc16(PC), Scf]],
        0x3F => &[&[Inc16(PC), Ccf]],

        // `PUSH reg16`
        0xC5 => push!(BC),
        0xD5 => push!(DE),
        0xE5 => push!(HL),
        0xF5 => push!(AF),

        // `POP reg16`
        0xC1 => pop!(BC),
        0xD1 => pop!(DE),
        0xE1 => pop!(HL),
        0xF1 => pop!(AF),

        // `JP imm16`
        0xC3 => jump!(Cond::None),

        // `JP cond, imm16`
        0xC2 => jump!(Cond::NotZero),
        0xCA => jump!(Cond::Zero),
        0xD2 => jump!(Cond::NotCarry),
        0xDA => jump!(Cond::Carry),

        // `JP HL`
        // TODO(solson): Remove the Inc16(PC)? It's only here for the inst_lengths test. We could
        // make every initial `Inc16(PC)` implicit.
        0xE9 => &[&[Inc16(PC), Move16(PC, HL)]],

        // `JR signed_imm8`
        0x18 => jump_relative!(Cond::None),

        // `JR cond, signed_imm8`
        0x20 => jump_relative!(Cond::NotZero),
        0x28 => jump_relative!(Cond::Zero),
        0x30 => jump_relative!(Cond::NotCarry),
        0x38 => jump_relative!(Cond::Carry),

        // `CALL imm16`
        0xCD => call!(Cond::None),

        // `CALL cond, imm16`
        0xC4 => call!(Cond::NotZero),
        0xCC => call!(Cond::Zero),
        0xD4 => call!(Cond::NotCarry),
        0xDC => call!(Cond::Carry),

        // `RST 0x{00,08,10,18,20,28,30,38}`
        0xC7 => rst!(0x00),
        0xCF => rst!(0x08),
        0xD7 => rst!(0x10),
        0xDF => rst!(0x18),
        0xE7 => rst!(0x20),
        0xEF => rst!(0x28),
        0xF7 => rst!(0x30),
        0xFF => rst!(0x38),

        // `RET`
        0xC9 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(SP)), Inc16(SP)],
            &[Read(RegHigh(Temp), Mem::Reg(SP)), Inc16(SP)],
            &[Move16(PC, Temp)],
        ],

        // `RET cond`
        0xC0 => ret_cond!(Cond::NotZero),
        0xC8 => ret_cond!(Cond::Zero),
        0xD0 => ret_cond!(Cond::NotCarry),
        0xD8 => ret_cond!(Cond::Carry),

        // `RETI`
        0xD9 => &[
            &[Inc16(PC)],
            &[Read(RegLow(Temp), Mem::Reg(SP)), Inc16(SP)],
            &[Read(RegHigh(Temp), Mem::Reg(SP)), Inc16(SP)],
            &[Move16(PC, Temp), EnableInterrupts],
        ],

        0x76 => &[&[Inc16(PC), Halt]],
        0xF3 => &[&[Inc16(PC), DisableInterruptsDelayed]],
        0xFB => &[&[Inc16(PC), EnableInterruptsDelayed]],
        0xCB => &[&[Inc16(PC), DecodePrefixed]],

        #[cfg(not(test))] opcode => panic!("inst microcode unimplemented: 0x{:02X}", opcode),
        #[cfg(test)] _ => &[&[Unimplemented]],
    }
}

macro_rules! alu_op {
    ($op:ident, $reg:expr) => (&[
        &[Inc16(PC), $op(Reg($reg))],
    ]);
}

macro_rules! bit_op {
    ($op:ident, $bit:expr, $reg:expr) => (&[
        &[Inc16(PC), $op($bit, Reg($reg))],
    ]);
}

macro_rules! alu_op_hl {
    ($op:ident) => (&[
        &[Inc16(PC)],
        &[Read(RegLow(Temp), Mem::Reg(HL)), $op(RegLow(Temp))],
        &[Write(Mem::Reg(HL), RegLow(Temp))],
    ]);
}

macro_rules! bit_op_hl {
    ($op:ident, $bit:expr) => (&[
        &[Inc16(PC)],
        &[Read(RegLow(Temp), Mem::Reg(HL)), $op($bit, RegLow(Temp))],
    ]);
}

macro_rules! bit_op_hl_mut {
    ($op:ident, $bit:expr) => (&[
        &[Inc16(PC)],
        &[Read(RegLow(Temp), Mem::Reg(HL)), $op($bit, RegLow(Temp))],
        &[Write(Mem::Reg(HL), RegLow(Temp))],
    ]);
}

pub fn microcode_cb_prefixed(opcode: u8) -> &'static [&'static [MicroInst]] {
    match opcode {
        0x00 => alu_op!(Rlc, B),
        0x01 => alu_op!(Rlc, C),
        0x02 => alu_op!(Rlc, D),
        0x03 => alu_op!(Rlc, E),
        0x04 => alu_op!(Rlc, H),
        0x05 => alu_op!(Rlc, L),
        0x07 => alu_op!(Rlc, A),

        0x08 => alu_op!(Rrc, B),
        0x09 => alu_op!(Rrc, C),
        0x0A => alu_op!(Rrc, D),
        0x0B => alu_op!(Rrc, E),
        0x0C => alu_op!(Rrc, H),
        0x0D => alu_op!(Rrc, L),
        0x0F => alu_op!(Rrc, A),

        0x10 => alu_op!(Rl, B),
        0x11 => alu_op!(Rl, C),
        0x12 => alu_op!(Rl, D),
        0x13 => alu_op!(Rl, E),
        0x14 => alu_op!(Rl, H),
        0x15 => alu_op!(Rl, L),
        0x17 => alu_op!(Rl, A),

        0x18 => alu_op!(Rr, B),
        0x19 => alu_op!(Rr, C),
        0x1A => alu_op!(Rr, D),
        0x1B => alu_op!(Rr, E),
        0x1C => alu_op!(Rr, H),
        0x1D => alu_op!(Rr, L),
        0x1F => alu_op!(Rr, A),

        0x20 => alu_op!(Sla, B),
        0x21 => alu_op!(Sla, C),
        0x22 => alu_op!(Sla, D),
        0x23 => alu_op!(Sla, E),
        0x24 => alu_op!(Sla, H),
        0x25 => alu_op!(Sla, L),
        0x27 => alu_op!(Sla, A),

        0x28 => alu_op!(Sra, B),
        0x29 => alu_op!(Sra, C),
        0x2A => alu_op!(Sra, D),
        0x2B => alu_op!(Sra, E),
        0x2C => alu_op!(Sra, H),
        0x2D => alu_op!(Sra, L),
        0x2F => alu_op!(Sra, A),

        0x30 => alu_op!(Swap, B),
        0x31 => alu_op!(Swap, C),
        0x32 => alu_op!(Swap, D),
        0x33 => alu_op!(Swap, E),
        0x34 => alu_op!(Swap, H),
        0x35 => alu_op!(Swap, L),
        0x37 => alu_op!(Swap, A),

        0x38 => alu_op!(Srl, B),
        0x39 => alu_op!(Srl, C),
        0x3A => alu_op!(Srl, D),
        0x3B => alu_op!(Srl, E),
        0x3C => alu_op!(Srl, H),
        0x3D => alu_op!(Srl, L),
        0x3F => alu_op!(Srl, A),

        0x40 => bit_op!(Bit, 0, B),
        0x41 => bit_op!(Bit, 0, C),
        0x42 => bit_op!(Bit, 0, D),
        0x43 => bit_op!(Bit, 0, E),
        0x44 => bit_op!(Bit, 0, H),
        0x45 => bit_op!(Bit, 0, L),
        0x47 => bit_op!(Bit, 0, A),

        0x48 => bit_op!(Bit, 1, B),
        0x49 => bit_op!(Bit, 1, C),
        0x4A => bit_op!(Bit, 1, D),
        0x4B => bit_op!(Bit, 1, E),
        0x4C => bit_op!(Bit, 1, H),
        0x4D => bit_op!(Bit, 1, L),
        0x4F => bit_op!(Bit, 1, A),

        0x50 => bit_op!(Bit, 2, B),
        0x51 => bit_op!(Bit, 2, C),
        0x52 => bit_op!(Bit, 2, D),
        0x53 => bit_op!(Bit, 2, E),
        0x54 => bit_op!(Bit, 2, H),
        0x55 => bit_op!(Bit, 2, L),
        0x57 => bit_op!(Bit, 2, A),

        0x58 => bit_op!(Bit, 3, B),
        0x59 => bit_op!(Bit, 3, C),
        0x5A => bit_op!(Bit, 3, D),
        0x5B => bit_op!(Bit, 3, E),
        0x5C => bit_op!(Bit, 3, H),
        0x5D => bit_op!(Bit, 3, L),
        0x5F => bit_op!(Bit, 3, A),

        0x60 => bit_op!(Bit, 4, B),
        0x61 => bit_op!(Bit, 4, C),
        0x62 => bit_op!(Bit, 4, D),
        0x63 => bit_op!(Bit, 4, E),
        0x64 => bit_op!(Bit, 4, H),
        0x65 => bit_op!(Bit, 4, L),
        0x67 => bit_op!(Bit, 4, A),

        0x68 => bit_op!(Bit, 5, B),
        0x69 => bit_op!(Bit, 5, C),
        0x6A => bit_op!(Bit, 5, D),
        0x6B => bit_op!(Bit, 5, E),
        0x6C => bit_op!(Bit, 5, H),
        0x6D => bit_op!(Bit, 5, L),
        0x6F => bit_op!(Bit, 5, A),

        0x70 => bit_op!(Bit, 6, B),
        0x71 => bit_op!(Bit, 6, C),
        0x72 => bit_op!(Bit, 6, D),
        0x73 => bit_op!(Bit, 6, E),
        0x74 => bit_op!(Bit, 6, H),
        0x75 => bit_op!(Bit, 6, L),
        0x77 => bit_op!(Bit, 6, A),

        0x78 => bit_op!(Bit, 7, B),
        0x79 => bit_op!(Bit, 7, C),
        0x7A => bit_op!(Bit, 7, D),
        0x7B => bit_op!(Bit, 7, E),
        0x7C => bit_op!(Bit, 7, H),
        0x7D => bit_op!(Bit, 7, L),
        0x7F => bit_op!(Bit, 7, A),

        0x80 => bit_op!(Res, 0, B),
        0x81 => bit_op!(Res, 0, C),
        0x82 => bit_op!(Res, 0, D),
        0x83 => bit_op!(Res, 0, E),
        0x84 => bit_op!(Res, 0, H),
        0x85 => bit_op!(Res, 0, L),
        0x87 => bit_op!(Res, 0, A),

        0x88 => bit_op!(Res, 1, B),
        0x89 => bit_op!(Res, 1, C),
        0x8A => bit_op!(Res, 1, D),
        0x8B => bit_op!(Res, 1, E),
        0x8C => bit_op!(Res, 1, H),
        0x8D => bit_op!(Res, 1, L),
        0x8F => bit_op!(Res, 1, A),

        0x90 => bit_op!(Res, 2, B),
        0x91 => bit_op!(Res, 2, C),
        0x92 => bit_op!(Res, 2, D),
        0x93 => bit_op!(Res, 2, E),
        0x94 => bit_op!(Res, 2, H),
        0x95 => bit_op!(Res, 2, L),
        0x97 => bit_op!(Res, 2, A),

        0x98 => bit_op!(Res, 3, B),
        0x99 => bit_op!(Res, 3, C),
        0x9A => bit_op!(Res, 3, D),
        0x9B => bit_op!(Res, 3, E),
        0x9C => bit_op!(Res, 3, H),
        0x9D => bit_op!(Res, 3, L),
        0x9F => bit_op!(Res, 3, A),

        0xA0 => bit_op!(Res, 4, B),
        0xA1 => bit_op!(Res, 4, C),
        0xA2 => bit_op!(Res, 4, D),
        0xA3 => bit_op!(Res, 4, E),
        0xA4 => bit_op!(Res, 4, H),
        0xA5 => bit_op!(Res, 4, L),
        0xA7 => bit_op!(Res, 4, A),

        0xA8 => bit_op!(Res, 5, B),
        0xA9 => bit_op!(Res, 5, C),
        0xAA => bit_op!(Res, 5, D),
        0xAB => bit_op!(Res, 5, E),
        0xAC => bit_op!(Res, 5, H),
        0xAD => bit_op!(Res, 5, L),
        0xAF => bit_op!(Res, 5, A),

        0xB0 => bit_op!(Res, 6, B),
        0xB1 => bit_op!(Res, 6, C),
        0xB2 => bit_op!(Res, 6, D),
        0xB3 => bit_op!(Res, 6, E),
        0xB4 => bit_op!(Res, 6, H),
        0xB5 => bit_op!(Res, 6, L),
        0xB7 => bit_op!(Res, 6, A),

        0xB8 => bit_op!(Res, 7, B),
        0xB9 => bit_op!(Res, 7, C),
        0xBA => bit_op!(Res, 7, D),
        0xBB => bit_op!(Res, 7, E),
        0xBC => bit_op!(Res, 7, H),
        0xBD => bit_op!(Res, 7, L),
        0xBF => bit_op!(Res, 7, A),

        0xC0 => bit_op!(Set, 0, B),
        0xC1 => bit_op!(Set, 0, C),
        0xC2 => bit_op!(Set, 0, D),
        0xC3 => bit_op!(Set, 0, E),
        0xC4 => bit_op!(Set, 0, H),
        0xC5 => bit_op!(Set, 0, L),
        0xC7 => bit_op!(Set, 0, A),

        0xC8 => bit_op!(Set, 1, B),
        0xC9 => bit_op!(Set, 1, C),
        0xCA => bit_op!(Set, 1, D),
        0xCB => bit_op!(Set, 1, E),
        0xCC => bit_op!(Set, 1, H),
        0xCD => bit_op!(Set, 1, L),
        0xCF => bit_op!(Set, 1, A),

        0xD0 => bit_op!(Set, 2, B),
        0xD1 => bit_op!(Set, 2, C),
        0xD2 => bit_op!(Set, 2, D),
        0xD3 => bit_op!(Set, 2, E),
        0xD4 => bit_op!(Set, 2, H),
        0xD5 => bit_op!(Set, 2, L),
        0xD7 => bit_op!(Set, 2, A),

        0xD8 => bit_op!(Set, 3, B),
        0xD9 => bit_op!(Set, 3, C),
        0xDA => bit_op!(Set, 3, D),
        0xDB => bit_op!(Set, 3, E),
        0xDC => bit_op!(Set, 3, H),
        0xDD => bit_op!(Set, 3, L),
        0xDF => bit_op!(Set, 3, A),

        0xE0 => bit_op!(Set, 4, B),
        0xE1 => bit_op!(Set, 4, C),
        0xE2 => bit_op!(Set, 4, D),
        0xE3 => bit_op!(Set, 4, E),
        0xE4 => bit_op!(Set, 4, H),
        0xE5 => bit_op!(Set, 4, L),
        0xE7 => bit_op!(Set, 4, A),

        0xE8 => bit_op!(Set, 5, B),
        0xE9 => bit_op!(Set, 5, C),
        0xEA => bit_op!(Set, 5, D),
        0xEB => bit_op!(Set, 5, E),
        0xEC => bit_op!(Set, 5, H),
        0xED => bit_op!(Set, 5, L),
        0xEF => bit_op!(Set, 5, A),

        0xF0 => bit_op!(Set, 6, B),
        0xF1 => bit_op!(Set, 6, C),
        0xF2 => bit_op!(Set, 6, D),
        0xF3 => bit_op!(Set, 6, E),
        0xF4 => bit_op!(Set, 6, H),
        0xF5 => bit_op!(Set, 6, L),
        0xF7 => bit_op!(Set, 6, A),

        0xF8 => bit_op!(Set, 7, B),
        0xF9 => bit_op!(Set, 7, C),
        0xFA => bit_op!(Set, 7, D),
        0xFB => bit_op!(Set, 7, E),
        0xFC => bit_op!(Set, 7, H),
        0xFD => bit_op!(Set, 7, L),
        0xFF => bit_op!(Set, 7, A),

        0x06 => alu_op_hl!(Rlc),
        0x0E => alu_op_hl!(Rrc),
        0x16 => alu_op_hl!(Rl),
        0x1E => alu_op_hl!(Rr),
        0x26 => alu_op_hl!(Sla),
        0x2E => alu_op_hl!(Sra),
        0x36 => alu_op_hl!(Swap),
        0x3E => alu_op_hl!(Srl),

        0x46 => bit_op_hl!(Bit, 0),
        0x4E => bit_op_hl!(Bit, 1),
        0x56 => bit_op_hl!(Bit, 2),
        0x5E => bit_op_hl!(Bit, 3),
        0x66 => bit_op_hl!(Bit, 4),
        0x6E => bit_op_hl!(Bit, 5),
        0x76 => bit_op_hl!(Bit, 6),
        0x7E => bit_op_hl!(Bit, 7),

        0x86 => bit_op_hl_mut!(Res, 0),
        0x8E => bit_op_hl_mut!(Res, 1),
        0x96 => bit_op_hl_mut!(Res, 2),
        0x9E => bit_op_hl_mut!(Res, 3),
        0xA6 => bit_op_hl_mut!(Res, 4),
        0xAE => bit_op_hl_mut!(Res, 5),
        0xB6 => bit_op_hl_mut!(Res, 6),
        0xBE => bit_op_hl_mut!(Res, 7),

        0xC6 => bit_op_hl_mut!(Set, 0),
        0xCE => bit_op_hl_mut!(Set, 1),
        0xD6 => bit_op_hl_mut!(Set, 2),
        0xDE => bit_op_hl_mut!(Set, 3),
        0xE6 => bit_op_hl_mut!(Set, 4),
        0xEE => bit_op_hl_mut!(Set, 5),
        0xF6 => bit_op_hl_mut!(Set, 6),
        0xFE => bit_op_hl_mut!(Set, 7),

        _ => unreachable!(),
    }
}

#[test]
fn inst_cycles() {
    for opcode in 0..=0xFF {
        if opcode == 0xCB { continue; }

        match microcode(opcode) {
            [[Unimplemented]] => continue,
            _ => {}
        }

        let total_cycles = microcode(opcode).len();
        let unconditional_cycles = microcode(opcode)
            .iter()
            .position(|ops| ops.iter()
            .any(|op| {
                match op { CheckCond(cond) => *cond != Cond::None, _ => false }
            }))
            .map(|cycles| cycles + 1);

        let (base_cycles, cond_cycles) = match unconditional_cycles {
            Some(cycles) => (cycles, total_cycles - cycles),
            None => (total_cycles, 0),
        };

        let expected_base_cycles = crate::cpu::inst::BASE_CYCLES[opcode as usize];
        assert_eq!(base_cycles * 4, expected_base_cycles, "inst 0x{:02X} base cycles", opcode);

        let expected_cond_cycles = crate::cpu::inst::CONDITIONAL_CYCLES[opcode as usize];
        assert_eq!(cond_cycles * 4, expected_cond_cycles, "inst 0x{:02X} cond cycles", opcode);
    }
}

#[test]
fn cb_inst_cycles() {
    for opcode in 0..=0xFF {
        let base_cycles = microcode_cb_prefixed(opcode).len() + 1;
        let expected_base_cycles = crate::cpu::inst::PREFIX_CB_BASE_CYCLES[opcode as usize];
        assert_eq!(base_cycles * 4, expected_base_cycles, "inst 0xCB 0x{:02X} cycles", opcode);
    }
}

#[test]
fn inst_lengths() {
    for opcode in 0..=0xFF {
        if opcode == 0xCB { continue; }

        match microcode(opcode) {
            [[Unimplemented]] => continue,
            _ => {}
        }
        let mut microcode_len = 0;
        for &cycle in microcode(opcode) {
            for micro_inst in cycle {
                match micro_inst {
                    Inc16(PC) => microcode_len += 1,
                    _ => {}
                }
            }
        }
        let expected_len = crate::cpu::inst::INSTRUCTION_LENGTH[opcode as usize];
        assert_eq!(microcode_len, expected_len, "inst 0x{:02X}", opcode);
    }
}
