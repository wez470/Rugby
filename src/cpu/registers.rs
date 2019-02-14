#[derive(Clone, Copy, Debug, Default)]
pub struct Register {
    pub high: u8,
    pub low: u8
}

impl Register {
    pub fn new(val: u16) -> Self {
        let mut reg = Register::default();
        reg.set(val);
        reg
    }

    pub fn set(&mut self, val: u16) {
        let [low, high] = val.to_le_bytes();
        self.low = low;
        self.high = high;
    }

    pub fn get(&self) -> u16 {
        u16::from_le_bytes([self.low, self.high])
    }

    pub fn inc(&mut self, val: i8) {
        let new_val = (self.get() as i32 + val as i32) as u16;
        self.set(new_val);
    }

    pub fn is_bit_set(&self, bit: u8) -> bool {
        self.get() & (1 << bit) != 0
    }

    pub fn set_bit(&mut self, bit: u8, set: bool) {
        let val = self.get();
        if set {
            self.set(val | (1 << bit));
        }
        else {
            self.set(val & !(1 << bit));
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Clone, Copy, Debug)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

/// Represents bit indexes of flags in the flags register.
// TODO(solson): Use BitFlags for this.
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Flag {
    Zero = 7,
    Sub = 6,
    HalfCarry = 5,
    Carry = 4,
}

#[derive(Clone, Copy, Debug)]
pub struct Registers {
    /// The 16-bit `AF` register, composed of two 8-bit registers:
    ///   * `A`, also known as the accumulator.
    ///   * `F`, the flags register.
    pub af: Register,

    /// The 16-bit `BC` register, composed of the two general-purpose 8-bit registers `B` and `C`.
    pub bc: Register,

    /// The 16-bit `DE` register, composed of the two general-purpose 8-bit registers `D` and `E`.
    /// Register containing registers 'D' and 'E'
    pub de: Register,

    /// The 16-bit `HL` register, composed of the two general-purpose 8-bit registers `H` and `L`.
    pub hl: Register,

    /// The 16-bit `SP` register, which contains the stack pointer.
    pub sp: Register,

    /// The 16-bit `PC` register, which contains the program counter.
    pub pc: Register,
}

impl Registers {
    pub fn new() -> Self {
        Self {
            pc: Register::new(0x0100),
            af: Register { high: 0x01, low: 0xB0 },
            bc: Register::new(0x0013),
            de: Register::new(0x00D8),
            hl: Register::new(0x014D),
            sp: Register::new(0xFFFE),
        }
    }

    pub fn set_8(&mut self, reg: Reg8, val: u8) {
        match reg {
            Reg8::A => self.af.high = val,
            Reg8::B => self.bc.high = val,
            Reg8::C => self.bc.low = val,
            Reg8::D => self.de.high = val,
            Reg8::E => self.de.low = val,
            Reg8::H => self.hl.high = val,
            Reg8::L => self.hl.low = val,
        }
    }

    pub fn get_8(&self, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => self.af.high,
            Reg8::B => self.bc.high,
            Reg8::C => self.bc.low,
            Reg8::D => self.de.high,
            Reg8::E => self.de.low,
            Reg8::H => self.hl.high,
            Reg8::L => self.hl.low,
        }
    }

    pub fn set_16(&mut self, reg: Reg16, val: u16) {
        match reg {
            // The four low bits of F, the flag register, must always be zero.
            Reg16::AF => self.af.set(val & 0xFFF0),
            Reg16::BC => self.bc.set(val),
            Reg16::DE => self.de.set(val),
            Reg16::HL => self.hl.set(val),
            Reg16::SP => self.sp.set(val),
            Reg16::PC => self.pc.set(val),
        }
    }

    pub fn get_16(&self, reg: Reg16) -> u16 {
        match reg {
            // The four low bits of F, the flag register, must always be zero.
            Reg16::AF => self.af.get() & 0xFFF0,
            Reg16::BC => self.bc.get(),
            Reg16::DE => self.de.get(),
            Reg16::HL => self.hl.get(),
            Reg16::SP => self.sp.get(),
            Reg16::PC => self.pc.get(),
        }
    }
}

#[test]
fn test_set_16_bit_reg() {
    let mut reg = Register::default();
    reg.set(60000);
    assert_eq!(reg.low, 96);
    assert_eq!(reg.high, 234);
}

#[test]
fn test_get_16_bit_reg() {
    let mut reg = Register::default();
    reg.low = 96;
    reg.high = 234;
    assert_eq!(reg.get(), 60000);
}

#[test]
fn test_inc() {
    let mut reg = Register::default();
    reg.set(0);
    reg.inc(1);
    assert_eq!(reg.get(), 1);
}