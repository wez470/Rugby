use reg_16::Reg16;
use memory::Memory;

pub const BASE_CYCLES: [usize; 0x100] = [
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

pub const INSTRUCTION_LENGTH: [usize; 0x100] = [
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
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1, // B
     1,  1,  3,  3,  3,  1,  2,  1,  1,  1,  3,  1,  3,  3,  2,  1, // C
     1,  1,  3,  0,  3,  1,  2,  1,  1,  1,  3,  0,  3,  0,  2,  1, // D
     2,  1,  2,  0,  0,  1,  2,  1,  2,  1,  3,  0,  0,  0,  2,  1, // E
     2,  1,  2,  1,  0,  1,  2,  1,  2,  1,  3,  1,  0,  0,  2,  1, // F
];

pub const PREFIX_CB_BASE_CYCLES: [usize; 0x100] = [
//   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 0
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 1
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 2
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 3
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 4
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 5
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 6
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 7
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 8
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 9
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // A
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // B
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // C
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // D
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // E
	 8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // F
];

// Every CB-prefixed instruction has length 2 (counting the CB opcode).
pub const PREFIX_CB_INSTRUCTION_LENGTH: usize = 2;

fn base_cycles(opcode: u8) -> usize {
    if opcode == 0xCB {
        PREFIX_CB_BASE_CYCLES[opcode as usize]
    } else {
        BASE_CYCLES[opcode as usize]
    }
}

fn instruction_length(opcode: u8) -> usize {
    if opcode == 0xCB {
        PREFIX_CB_INSTRUCTION_LENGTH
    } else {
        INSTRUCTION_LENGTH[opcode as usize]
    }
}

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

#[derive(Clone)]
pub struct Cpu {
    /// Registers
    reg_af: Reg16,
    reg_bc: Reg16,
    reg_de: Reg16,
    reg_hl: Reg16,
    reg_sp: Reg16,
    reg_pc: Reg16,

    /// RAM and ROM
    memory: Memory,
    rom: Box<[u8]>,

    /// The location of the start of the currently-executing instruction.
    base_pc: usize,

    /// A running total of the number of cycles taken in execution so far.
    cycles: usize,

    /// True if interrupts can currently execute.
    interrupts_enabled: bool,

    /// True if the previous instruction was DI, which disables interrupts after the next
    /// instruction.
    pending_disable_interrupts: bool,

    /// True if the previous instruction was EI, which enables interrupts after the next
    /// instruction.
    pending_enable_interrupts: bool,
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
            interrupts_enabled: false,
            pending_disable_interrupts: false,
            pending_enable_interrupts: false,
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
        let pending_enable_interrupts = self.pending_enable_interrupts;
        let pending_disable_interrupts = self.pending_disable_interrupts;
        self.pending_enable_interrupts = false;
        self.pending_disable_interrupts = false;

        self.base_pc = self.reg_pc.get() as usize;
        let opcode = self.rom[self.base_pc];
        let instruction_len = instruction_length(opcode);
        self.cycles += base_cycles(opcode);

        print!("{:04X}:", self.base_pc);
        for &byte in &self.rom[self.base_pc .. (self.base_pc + instruction_len)] {
            print!(" {:02X}", byte);
        }
        println!();

        self.reg_pc.inc(instruction_len as i8);

        match opcode {
            0x00 => {}, // No-op
            0x06 => self.load_imm8(Regs_8::B),
            0x11 => self.load_imm16(Regs_16::DE),
            0x18 => self.jr_r8(),
            0x28 => self.jr_z_signed_8(),
            0x3E => self.load_imm8(Regs_8::A),
            0x40 => self.copy_reg_8(Regs_8::B, Regs_8::B),
            0x41 => self.copy_reg_8(Regs_8::B, Regs_8::C),
            0x42 => self.copy_reg_8(Regs_8::B, Regs_8::D),
            0x43 => self.copy_reg_8(Regs_8::B, Regs_8::E),
            0x44 => self.copy_reg_8(Regs_8::B, Regs_8::H),
            0x45 => self.copy_reg_8(Regs_8::B, Regs_8::L),
            0x47 => self.copy_reg_8(Regs_8::B, Regs_8::A),
            0x48 => self.copy_reg_8(Regs_8::C, Regs_8::B),
            0x49 => self.copy_reg_8(Regs_8::C, Regs_8::C),
            0x4A => self.copy_reg_8(Regs_8::C, Regs_8::D),
            0x4B => self.copy_reg_8(Regs_8::C, Regs_8::E),
            0x4C => self.copy_reg_8(Regs_8::C, Regs_8::H),
            0x4D => self.copy_reg_8(Regs_8::C, Regs_8::L),
            0x4F => self.copy_reg_8(Regs_8::C, Regs_8::A),
            0x50 => self.copy_reg_8(Regs_8::D, Regs_8::B),
            0x51 => self.copy_reg_8(Regs_8::D, Regs_8::C),
            0x52 => self.copy_reg_8(Regs_8::D, Regs_8::D),
            0x53 => self.copy_reg_8(Regs_8::D, Regs_8::E),
            0x54 => self.copy_reg_8(Regs_8::D, Regs_8::H),
            0x55 => self.copy_reg_8(Regs_8::D, Regs_8::L),
            0x57 => self.copy_reg_8(Regs_8::D, Regs_8::A),
            0x58 => self.copy_reg_8(Regs_8::E, Regs_8::B),
            0x59 => self.copy_reg_8(Regs_8::E, Regs_8::C),
            0x5A => self.copy_reg_8(Regs_8::E, Regs_8::D),
            0x5B => self.copy_reg_8(Regs_8::E, Regs_8::E),
            0x5C => self.copy_reg_8(Regs_8::E, Regs_8::H),
            0x5D => self.copy_reg_8(Regs_8::E, Regs_8::L),
            0x5F => self.copy_reg_8(Regs_8::E, Regs_8::A),
            0x60 => self.copy_reg_8(Regs_8::H, Regs_8::B),
            0x61 => self.copy_reg_8(Regs_8::H, Regs_8::C),
            0x62 => self.copy_reg_8(Regs_8::H, Regs_8::D),
            0x63 => self.copy_reg_8(Regs_8::H, Regs_8::E),
            0x64 => self.copy_reg_8(Regs_8::H, Regs_8::H),
            0x65 => self.copy_reg_8(Regs_8::H, Regs_8::L),
            0x67 => self.copy_reg_8(Regs_8::H, Regs_8::A),
            0x68 => self.copy_reg_8(Regs_8::L, Regs_8::B),
            0x69 => self.copy_reg_8(Regs_8::L, Regs_8::C),
            0x6A => self.copy_reg_8(Regs_8::L, Regs_8::D),
            0x6B => self.copy_reg_8(Regs_8::L, Regs_8::E),
            0x6C => self.copy_reg_8(Regs_8::L, Regs_8::H),
            0x6D => self.copy_reg_8(Regs_8::L, Regs_8::L),
            0x6F => self.copy_reg_8(Regs_8::L, Regs_8::A),
            0xA8 => self.xor(Regs_8::B),
            0xA9 => self.xor(Regs_8::C),
            0xAA => self.xor(Regs_8::D),
            0xAB => self.xor(Regs_8::E),
            0xAC => self.xor(Regs_8::H),
            0xAD => self.xor(Regs_8::L),
            0xAF => self.xor(Regs_8::A),
            0xC3 => self.load_imm16(Regs_16::PC), // Note: this is a jump.
            0xCD => self.call(),
            0xE0 => self.store_high_a8(Regs_8::A),
            0xEA => self.store_a16(Regs_8::A),
            0xF0 => self.load_high_a8(Regs_8::A),
            0xF3 => self.pending_disable_interrupts = true,
            0xFB => self.pending_enable_interrupts = true,
            0xFE => self.cp(),

            0xCB => {
                let opcode_after_cb = self.rom[self.base_pc + 1];
                match opcode_after_cb {
                    _ => {
                        println!("unimplemented: CB {:02X}", opcode_after_cb);
                    }
                }
            }

            _ => {
                println!("unimplemented: {:02X}", opcode);
            }
        };

        if pending_enable_interrupts {
            self.interrupts_enabled = true;
        }

        if pending_disable_interrupts {
            self.interrupts_enabled = false;
        }
    }

    pub fn step_n(&mut self, steps: usize) {
        for _ in 0..steps {
            self.step();
        }
    }

    /// Load immediate 8-bit data into the given register.
    fn copy_reg_8(&mut self, dest: Regs_8, src: Regs_8) {
        let n = self.get_reg_8(src);
        self.set_reg_8(dest, n);
    }

    /// Load immediate 8-bit data into the given register.
    fn load_imm8(&mut self, reg: Regs_8) {
        let n = self.rom[self.base_pc + 1];
        self.set_reg_8(reg, n);
    }

    /// Load immediate 16-bit data into the given register.
    fn load_imm16(&mut self, reg: Regs_16) {
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

    /// Load an 8-bit register value from an immediate 8-bit memory address, after adding 0xFF00 to
    /// the address.
    fn load_high_a8(&mut self, reg: Regs_8) {
        let addr_low = self.rom[self.base_pc + 1];
        let addr = 0xFF00u16 | addr_low as u16;
        let val = self.memory.mem[addr as usize];
        self.set_reg_8(reg, val);
    }

    /// Store an 8-bit register value in an immediate 8-bit memory address, after adding 0xFF00 to
    /// the address.
    fn store_high_a8(&mut self, reg: Regs_8) {
        let addr_low = self.rom[self.base_pc + 1];
        let addr = 0xFF00u16 | addr_low as u16;
        self.memory.mem[addr as usize] = self.get_reg_8(reg);
    }

    /// Store an 8-bit register value in an immediate 16-bit memory address.
    fn store_a16(&mut self, reg: Regs_8) {
        let low = self.rom[self.base_pc + 1] as u16;
        let high = self.rom[self.base_pc + 2] as u16;
        self.memory.mem[((high << 8) | low) as usize] = self.get_reg_8(reg);
    }

    fn cp(&mut self) {
        let a = self.reg_af.high;
        let n = self.rom[self.base_pc + 1];
        self.set_zero_flag(a == n);
        self.set_sub_flag(true);
        self.set_carry_flag(a < n);
        self.set_half_carry_flag(Cpu::get_sub_half_carry(a, n));
    }

    fn call(&mut self) {
        let next_instruction_addr = self.reg_pc.get();
        self.push(next_instruction_addr);
        self.load_imm16(Regs_16::PC);
    }

    fn push(&mut self, val: u16) {
        self.reg_sp.inc(-2);
        let addr = self.reg_sp.get() as usize;
        self.memory.mem[addr] = val as u8; // low
        self.memory.mem[addr + 1] = (val >> 8) as u8; // high
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
    use std::fmt::{Debug, UpperHex};
    use std::mem;

    fn setup(rom: Vec<u8>) -> (Cpu, Cpu) {
        let mut cpu = Cpu::new(rom.into_boxed_slice(), Memory::new());
        cpu.reg_pc.set(0);
        (cpu.clone(), cpu)
    }

    /// Check if the actual and expected results are the same, pretty-printing any differences, and
    /// panicking (failing the test) if there are any differences.
    fn check_diff(actual: &Cpu, expected: &Cpu) {
        let mut same = true;
        same &= diff_hex("AF register", &actual.reg_af.get(), &expected.reg_af.get());
        same &= diff_hex("BC register", &actual.reg_bc.get(), &expected.reg_bc.get());
        same &= diff_hex("DE register", &actual.reg_de.get(), &expected.reg_de.get());
        same &= diff_hex("HL register", &actual.reg_hl.get(), &expected.reg_hl.get());
        same &= diff_hex("SP register", &actual.reg_sp.get(), &expected.reg_sp.get());
        same &= diff_hex("PC register", &actual.reg_pc.get(), &expected.reg_pc.get());
        same &= diff("cycles", &actual.cycles, &expected.cycles);
        same &= diff(
            "interrupts_enabled",
            &actual.interrupts_enabled,
            &expected.interrupts_enabled
        );
        same &= diff(
            "pending_disable_interrupts",
            &actual.pending_disable_interrupts,
            &expected.pending_disable_interrupts
        );
        same &= diff(
            "pending_enable_interrupts",
            &actual.pending_enable_interrupts,
            &expected.pending_enable_interrupts
        );

        let actual_mem = actual.memory.mem.iter();
        let expected_mem = expected.memory.mem.iter();
        for (i, (actual_cell, expected_cell)) in actual_mem.zip(expected_mem).enumerate() {
            let name = format!("memory location 0x{:02X}", i);
            same &= diff_hex(&name, actual_cell, expected_cell);
        }

        let actual_rom = actual.rom.iter();
        let expected_rom = expected.rom.iter();
        for (i, (actual_cell, expected_cell)) in actual_rom.zip(expected_rom).enumerate() {
            let name = format!("ROM location 0x{:02X}", i);
            same &= diff_hex(&name, actual_cell, expected_cell);
        }

        if !same {
            panic!("actual and expected results differ");
        }
    }

    /// Returns whether the actual and expected numbers are the same. Pretty-prints the numbers in
    /// hex if they differ.
    fn diff_hex<T: Debug + Eq + UpperHex>(name: &str, actual: &T, expected: &T) -> bool {
        let same = actual == expected;
        if !same {
            let width = mem::size_of::<T>() * 2; // Number of hex digits for type T.
            println!("\ndifference in {}:", name);
            println!("  actual:   0x{0:01$X} ({0:?})", actual, width);
            println!("  expected: 0x{0:01$X} ({0:?})", expected, width);
        }
        same
    }

    /// Returns whether the actual and expected values are the same. Pretty-prints the values if
    /// they differ.
    fn diff<T: Debug + Eq>(name: &str, actual: &T, expected: &T) -> bool {
        let same = actual == expected;
        if !same {
            println!("\ndifference in {}:", name);
            println!("  actual:   {:?}", actual);
            println!("  expected: {:?}", expected);
        }
        same
    }

    #[test]
    fn test_nop() {
        let (mut actual, mut expected) = setup(vec![0x00]); // nop
        actual.step();

        expected.set_reg_16(Regs_16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_reg_8() {
        let (mut actual, mut expected) = setup(vec![
            0x3E, 0x13, // ld a, 0x13
            0x06, 0x42, // ld b, 0x42
        ]);
        actual.set_reg_8(Regs_8::A, 0);
        actual.set_reg_8(Regs_8::B, 0);
        actual.step_n(2);

        expected.set_reg_8(Regs_8::A, 0x13);
        expected.set_reg_8(Regs_8::B, 0x42);
        expected.set_reg_16(Regs_16::PC, 4);
        expected.cycles = 16;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_reg_16() {
        let (mut actual, mut expected) = setup(vec![
            0x11, 0x34, 0x12, // ld de, 0x1234
        ]);
        actual.set_reg_16(Regs_16::DE, 0);
        actual.step();

        expected.set_reg_16(Regs_16::DE, 0x1234);
        expected.set_reg_16(Regs_16::PC, 3);
        expected.cycles = 12;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_jp() {
        let (mut actual, mut expected) = setup(vec![
            0xC3, 0x34, 0x12, // jp 0x1234
        ]);
        actual.step();

        expected.set_reg_16(Regs_16::PC, 0x1234);
        expected.cycles = 16;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_di() {
        let (mut actual, mut expected) = setup(vec![
            0xF3, 0x00, // di; nop
        ]);
        actual.interrupts_enabled = true;
        actual.step(); // di

        // After DI, interrupts still aren't disabled until the next instruction executes.
        expected.interrupts_enabled = true;
        expected.pending_disable_interrupts = true;
        expected.set_reg_16(Regs_16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);

        println!("# Step 2");
        actual.step(); // nop

        // Now the instruction after DI has executed, so DI takes effect.
        expected.interrupts_enabled = false;
        expected.pending_disable_interrupts = false;
        expected.set_reg_16(Regs_16::PC, 2);
        expected.cycles = 8;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_ei() {
        let (mut actual, mut expected) = setup(vec![
            0xFB, 0x00, // ei; nop
        ]);
        actual.interrupts_enabled = false;
        actual.step(); // ei

        // After EI, interrupts still aren't disabled until the next instruction executes.
        expected.interrupts_enabled = false;
        expected.pending_enable_interrupts = true;
        expected.set_reg_16(Regs_16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);

        println!("# Step 2");
        actual.step(); // nop

        // Now the instruction after EI has executed, so EI takes effect.
        expected.interrupts_enabled = true;
        expected.pending_enable_interrupts = false;
        expected.set_reg_16(Regs_16::PC, 2);
        expected.cycles = 8;
        check_diff(&actual, &expected);
    }
}
