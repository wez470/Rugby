use reg_16::Reg16;
use memory::Memory;

pub const BASE_CYCLES: [i32; 0x100] = [
//     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
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
       8, 12, 12, 16, 12, 16,  8, 16,  8, 16, 12,  4, 12, 24,  8, 16, // C
       8, 12, 12,  0, 12, 16,  8, 16,  8, 16, 12,  0, 12,  0,  8, 16, // D
      12, 12,  8,  0,  0, 16,  8, 16, 16,  4, 16,  0,  0,  0,  8, 16, // E
      12, 12,  8,  4,  0, 16,  8, 16, 12,  8, 16,  4,  0,  0,  8, 16, // F
];

pub const INSTRUCTION_LENGTH: [usize; 0x100] = [
//     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
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
       1,  1,  3,  3,  3,  1,  2,  1,  1,  1,  3,  1,  3,  3,  2,  1, // C
       1,  1,  3,  0,  3,  1,  2,  1,  1,  1,  3,  0,  3,  0,  2,  1, // D
       2,  1,  2,  0,  0,  1,  2,  1,  2,  1,  3,  0,  0,  0,  2,  1, // E
       2,  1,  2,  1,  0,  1,  2,  1,  2,  1,  3,  1,  0,  0,  2,  1, // F
];

pub enum Regs_8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

pub enum Regs_16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

pub struct Cpu {
    // Registers
    reg_af: Reg16,
    reg_bc: Reg16,
    reg_de: Reg16,
    reg_hl: Reg16,
    reg_sp: Reg16,
    reg_pc: Reg16,

    // RAM and ROM
    memory: Memory,
    rom: Box<[u8]>,

    /// The location of the start of the currently-executing instruction.
    base_pc: usize,

    /// A running total of the number of cycles taken in execution so far.
    cycles: usize,
}

impl Cpu {
    pub fn new(rom: Box<[u8]>, mem: Memory) -> Cpu {
        let mut cpu = Cpu {
            reg_af: Reg16::default(),
            reg_bc: Reg16::default(),
            reg_de: Reg16::default(),
            reg_hl: Reg16::default(),
            reg_sp: Reg16::default(),
            reg_pc: Reg16::default(),
            memory: mem,
            rom: rom,
            base_pc: 0,
            cycles: 0,
        };
        cpu.reset();
        cpu
    }

    pub fn reset(&mut self) {
        self.reg_pc.set(0x0100);
        self.reg_af.high = 0x01;
        self.reg_af.low = 0xB0;
        self.reg_bc.set(0x0013);
        self.reg_de.set(0x00D8);
        self.reg_hl.set(0x014D);
        self.reg_sp.set(0xFFFE);
    }

    pub fn step(&mut self) {
        self.base_pc = self.reg_pc.get() as usize;
        let opcode = self.rom[self.base_pc];
        let instruction_len = INSTRUCTION_LENGTH[opcode as usize];
        self.cycles += BASE_CYCLES[opcode as usize] as usize;

        print!("{:04X}:", self.base_pc);
        for &byte in &self.rom[self.base_pc .. (self.base_pc + instruction_len)] {
            print!(" {:02X}", byte);
        }
        println!();

        self.reg_pc.inc(instruction_len as i8);

        match opcode {
            0x00 => {}, // No-op
            0x06 => self.ld_8(Regs_8::B),
            0x11 => self.ld_16(Regs_16::DE),
            0x18 => self.jr_r8(),
            0x28 => self.jr_z_signed_8(),
            0x3E => self.ld_8(Regs_8::A),
            0xA8 => self.xor(Regs_8::B),
            0xA9 => self.xor(Regs_8::C),
            0xAA => self.xor(Regs_8::D),
            0xAB => self.xor(Regs_8::E),
            0xAC => self.xor(Regs_8::H),
            0xAD => self.xor(Regs_8::L),
            0xAF => self.xor(Regs_8::A),
            0xC3 => self.ld_16(Regs_16::PC), // Note: this is a jump.
            0xFE => self.cp(),

            _ => {
                println!("{:02X} Unimplemented", opcode);
            }
        };
    }

    pub fn step_n(&mut self, steps: usize) {
        for _ in 0..steps {
            self.step();
        }
    }

    fn ld_8(&mut self, reg: Regs_8) {
        let n = self.rom[self.base_pc + 1];
        self.set_reg_8(reg, n);
    }

    fn ld_16(&mut self, reg: Regs_16) {
        let low = self.rom[self.base_pc + 1] as u16;
        let high = self.rom[self.base_pc + 2] as u16;
        self.set_reg_16(reg, (high << 8) | low);
    }

    fn jr_r8(&mut self) {
        let n = self.rom[self.base_pc + 1] as i8;
        self.reg_pc.inc(n);
    }

    fn jr_z_signed_8(&mut self) {
        if self.get_zero_flag() {
            self.cycles += 4;
            let val = self.rom[self.base_pc + 1] as i8;
            self.reg_pc.inc(val);
        }
    }

    fn xor(&mut self, reg: Regs_8) {
        let n = self.get_reg_8(reg);
        let result = self.reg_af.high ^ n;
        self.set_zero_flag(result == 0);
        self.set_sub_flag(false);
        self.set_half_carry_flag(false);
        self.set_carry_flag(false);
    }

    fn cp(&mut self) {
        let a = self.reg_af.high;
        let n = self.rom[self.base_pc + 1];
        self.set_zero_flag(a == n);
        self.set_sub_flag(true);
        self.set_carry_flag(a < n);
        self.set_half_carry_flag(Cpu::get_sub_half_carry(a, n));
    }

    fn get_add_half_carry(first: u8, second: u8) -> bool {
       ((first & 0xf) + (second & 0xf)) & 0x10 == 0x10
    }

    fn get_sub_half_carry(first: u8, second: u8) -> bool {
       (first & 0xf) < (second & 0xf)
    }

    fn set_zero_flag(&mut self, set: bool) {
        self.reg_af.set_bit_8(set);
    }

    fn set_sub_flag(&mut self, set: bool) {
        self.reg_af.set_bit_7(set);
    }

    fn set_half_carry_flag(&mut self, set: bool) {
        self.reg_af.set_bit_6(set);
    }

    fn set_carry_flag(&mut self, set: bool) {
        self.reg_af.set_bit_5(set);
    }

    fn get_zero_flag(&self) -> bool {
        self.reg_af.is_bit_8_set()
    }

    fn set_reg_8(&mut self, reg: Regs_8, val: u8) {
        match reg {
            Regs_8::A => self.reg_af.high = val,
            Regs_8::F => self.reg_af.low = val,
            Regs_8::B => self.reg_bc.high = val,
            Regs_8::C => self.reg_bc.low = val,
            Regs_8::D => self.reg_de.high = val,
            Regs_8::E => self.reg_de.low = val,
            Regs_8::H => self.reg_hl.high = val,
            Regs_8::L => self.reg_hl.low = val,
        }
    }

    fn get_reg_8(&self, reg: Regs_8) -> u8 {
         match reg {
            Regs_8::A => self.reg_af.high,
            Regs_8::F => self.reg_af.low,
            Regs_8::B => self.reg_bc.high,
            Regs_8::C => self.reg_bc.low,
            Regs_8::D => self.reg_de.high,
            Regs_8::E => self.reg_de.low,
            Regs_8::H => self.reg_hl.high,
            Regs_8::L => self.reg_hl.low,
        }
    }

    fn set_reg_16(&mut self, reg: Regs_16, val: u16) {
        match reg {
            Regs_16::AF => self.reg_af.set(val),
            Regs_16::BC => self.reg_bc.set(val),
            Regs_16::DE => self.reg_de.set(val),
            Regs_16::HL => self.reg_hl.set(val),
            Regs_16::SP => self.reg_sp.set(val),
            Regs_16::PC => self.reg_pc.set(val),
        }
    }

    fn get_reg_16(&self, reg: Regs_16) -> u16 {
         match reg {
            Regs_16::AF => self.reg_af.get(),
            Regs_16::BC => self.reg_bc.get(),
            Regs_16::DE => self.reg_de.get(),
            Regs_16::HL => self.reg_hl.get(),
            Regs_16::SP => self.reg_sp.get(),
            Regs_16::PC => self.reg_pc.get(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::Memory;

    fn setup(rom: Vec<u8>) -> Cpu {
        let mut cpu = Cpu::new(rom.into_boxed_slice(), Memory::new());
        cpu.reg_pc.set(0);
        cpu
    }

    #[test]
    fn test_nop() {
        let mut cpu = setup(vec![0x00]); // nop
        cpu.step();
        assert_eq!(cpu.reg_pc.get(), 1);
        assert_eq!(cpu.cycles, 4);
    }

    #[test]
    fn test_reg_8() {
        let mut cpu = setup(vec![
            0x3E, 0x13, // ld a, 0x13
            0x06, 0x42, // ld b, 0x42
        ]);
        cpu.set_reg_8(Regs_8::A, 0);
        cpu.set_reg_8(Regs_8::B, 0);
        cpu.step_n(2);
        assert_eq!(cpu.get_reg_8(Regs_8::A), 0x13);
        assert_eq!(cpu.get_reg_8(Regs_8::B), 0x42);
    }

    #[test]
    fn test_reg_16() {
        let mut cpu = setup(vec![0x11, 0x34, 0x12]); // ld de, 0x1234
        cpu.set_reg_16(Regs_16::DE, 0);
        cpu.step();
        assert_eq!(cpu.get_reg_16(Regs_16::DE), 0x1234);
    }

    #[test]
    fn test_jp() {
        let mut cpu = setup(vec![0xC3, 0x34, 0x12]); // jp 0x1234
        cpu.step();
        assert_eq!(cpu.cycles, 16);
        assert_eq!(cpu.reg_pc.get(), 0x1234);
    }
}
