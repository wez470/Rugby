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

pub struct Cpu {
    reg_af: Reg16,
    reg_bc: Reg16,
    reg_de: Reg16,
    reg_hl: Reg16,
    reg_sp: Reg16,
    reg_pc: Reg16,
    memory: Memory,
    rom: Box<[u8]>,
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

    pub fn run(&mut self) {
        let opcode = self.rom[self.reg_pc.get() as usize];
        println!("{:02X}", opcode);

        let mut cycles = 0;
        cycles += BASE_CYCLES[opcode as usize];

        match opcode {
            0x00 => self.nop(),
            0x01 => { println!("TODO: {:02X}", opcode) }
            0x02 => { println!("TODO: {:02X}", opcode) }
            0x03 => { println!("TODO: {:02X}", opcode) }
            0x04 => { println!("TODO: {:02X}", opcode) }
            0x05 => { println!("TODO: {:02X}", opcode) }
            0x06 => { println!("TODO: {:02X}", opcode) }
            0x07 => { println!("TODO: {:02X}", opcode) }
            0x08 => { println!("TODO: {:02X}", opcode) }
            0x09 => { println!("TODO: {:02X}", opcode) }
            0x0A => { println!("TODO: {:02X}", opcode) }
            0x0B => { println!("TODO: {:02X}", opcode) }
            0x0C => { println!("TODO: {:02X}", opcode) }
            0x0D => { println!("TODO: {:02X}", opcode) }
            0x0E => { println!("TODO: {:02X}", opcode) }
            0x0F => { println!("TODO: {:02X}", opcode) }

            0x10 => { println!("TODO: {:02X}", opcode) }
            0x11 => self.reg_de = self.ld_16(),
            0x12 => { println!("TODO: {:02X}", opcode) }
            0x13 => { println!("TODO: {:02X}", opcode) }
            0x14 => { println!("TODO: {:02X}", opcode) }
            0x15 => { println!("TODO: {:02X}", opcode) }
            0x16 => { println!("TODO: {:02X}", opcode) }
            0x17 => { println!("TODO: {:02X}", opcode) }
            0x18 => { println!("TODO: {:02X}", opcode) }
            0x19 => { println!("TODO: {:02X}", opcode) }
            0x1A => { println!("TODO: {:02X}", opcode) }
            0x1B => { println!("TODO: {:02X}", opcode) }
            0x1C => { println!("TODO: {:02X}", opcode) }
            0x1D => { println!("TODO: {:02X}", opcode) }
            0x1E => { println!("TODO: {:02X}", opcode) }
            0x1F => { println!("TODO: {:02X}", opcode) }

            0x20 => { println!("TODO: {:02X}", opcode) }
            0x21 => { println!("TODO: {:02X}", opcode) }
            0x22 => { println!("TODO: {:02X}", opcode) }
            0x23 => { println!("TODO: {:02X}", opcode) }
            0x24 => { println!("TODO: {:02X}", opcode) }
            0x25 => { println!("TODO: {:02X}", opcode) }
            0x26 => { println!("TODO: {:02X}", opcode) }
            0x27 => { println!("TODO: {:02X}", opcode) }
            0x28 => { println!("TODO: {:02X}", opcode) }
            0x29 => { println!("TODO: {:02X}", opcode) }
            0x2A => { println!("TODO: {:02X}", opcode) }
            0x2B => { println!("TODO: {:02X}", opcode) }
            0x2C => { println!("TODO: {:02X}", opcode) }
            0x2D => { println!("TODO: {:02X}", opcode) }
            0x2E => { println!("TODO: {:02X}", opcode) }
            0x2F => { println!("TODO: {:02X}", opcode) }

            0x30 => { println!("TODO: {:02X}", opcode) }
            0x31 => { println!("TODO: {:02X}", opcode) }
            0x32 => { println!("TODO: {:02X}", opcode) }
            0x33 => { println!("TODO: {:02X}", opcode) }
            0x34 => { println!("TODO: {:02X}", opcode) }
            0x35 => { println!("TODO: {:02X}", opcode) }
            0x36 => { println!("TODO: {:02X}", opcode) }
            0x37 => { println!("TODO: {:02X}", opcode) }
            0x38 => { println!("TODO: {:02X}", opcode) }
            0x39 => { println!("TODO: {:02X}", opcode) }
            0x3A => { println!("TODO: {:02X}", opcode) }
            0x3B => { println!("TODO: {:02X}", opcode) }
            0x3C => { println!("TODO: {:02X}", opcode) }
            0x3D => { println!("TODO: {:02X}", opcode) }
            0x3E => { println!("TODO: {:02X}", opcode) }
            0x3F => { println!("TODO: {:02X}", opcode) }

            0x40 => { println!("TODO: {:02X}", opcode) }
            0x41 => { println!("TODO: {:02X}", opcode) }
            0x42 => { println!("TODO: {:02X}", opcode) }
            0x43 => { println!("TODO: {:02X}", opcode) }
            0x44 => { println!("TODO: {:02X}", opcode) }
            0x45 => { println!("TODO: {:02X}", opcode) }
            0x46 => { println!("TODO: {:02X}", opcode) }
            0x47 => { println!("TODO: {:02X}", opcode) }
            0x48 => { println!("TODO: {:02X}", opcode) }
            0x49 => { println!("TODO: {:02X}", opcode) }
            0x4A => { println!("TODO: {:02X}", opcode) }
            0x4B => { println!("TODO: {:02X}", opcode) }
            0x4C => { println!("TODO: {:02X}", opcode) }
            0x4D => { println!("TODO: {:02X}", opcode) }
            0x4E => { println!("TODO: {:02X}", opcode) }
            0x4F => { println!("TODO: {:02X}", opcode) }

            0x50 => { println!("TODO: {:02X}", opcode) }
            0x51 => { println!("TODO: {:02X}", opcode) }
            0x52 => { println!("TODO: {:02X}", opcode) }
            0x53 => { println!("TODO: {:02X}", opcode) }
            0x54 => { println!("TODO: {:02X}", opcode) }
            0x55 => { println!("TODO: {:02X}", opcode) }
            0x56 => { println!("TODO: {:02X}", opcode) }
            0x57 => { println!("TODO: {:02X}", opcode) }
            0x58 => { println!("TODO: {:02X}", opcode) }
            0x59 => { println!("TODO: {:02X}", opcode) }
            0x5A => { println!("TODO: {:02X}", opcode) }
            0x5B => { println!("TODO: {:02X}", opcode) }
            0x5C => { println!("TODO: {:02X}", opcode) }
            0x5D => { println!("TODO: {:02X}", opcode) }
            0x5E => { println!("TODO: {:02X}", opcode) }
            0x5F => { println!("TODO: {:02X}", opcode) }

            0x60 => { println!("TODO: {:02X}", opcode) }
            0x61 => { println!("TODO: {:02X}", opcode) }
            0x62 => { println!("TODO: {:02X}", opcode) }
            0x63 => { println!("TODO: {:02X}", opcode) }
            0x64 => { println!("TODO: {:02X}", opcode) }
            0x65 => { println!("TODO: {:02X}", opcode) }
            0x66 => { println!("TODO: {:02X}", opcode) }
            0x67 => { println!("TODO: {:02X}", opcode) }
            0x68 => { println!("TODO: {:02X}", opcode) }
            0x69 => { println!("TODO: {:02X}", opcode) }
            0x6A => { println!("TODO: {:02X}", opcode) }
            0x6B => { println!("TODO: {:02X}", opcode) }
            0x6C => { println!("TODO: {:02X}", opcode) }
            0x6D => { println!("TODO: {:02X}", opcode) }
            0x6E => { println!("TODO: {:02X}", opcode) }
            0x6F => { println!("TODO: {:02X}", opcode) }

            0x70 => { println!("TODO: {:02X}", opcode) }
            0x71 => { println!("TODO: {:02X}", opcode) }
            0x72 => { println!("TODO: {:02X}", opcode) }
            0x73 => { println!("TODO: {:02X}", opcode) }
            0x74 => { println!("TODO: {:02X}", opcode) }
            0x75 => { println!("TODO: {:02X}", opcode) }
            0x76 => { println!("TODO: {:02X}", opcode) }
            0x77 => { println!("TODO: {:02X}", opcode) }
            0x78 => { println!("TODO: {:02X}", opcode) }
            0x79 => { println!("TODO: {:02X}", opcode) }
            0x7A => { println!("TODO: {:02X}", opcode) }
            0x7B => { println!("TODO: {:02X}", opcode) }
            0x7C => { println!("TODO: {:02X}", opcode) }
            0x7D => { println!("TODO: {:02X}", opcode) }
            0x7E => { println!("TODO: {:02X}", opcode) }
            0x7F => { println!("TODO: {:02X}", opcode) }

            0x80 => { println!("TODO: {:02X}", opcode) }
            0x81 => { println!("TODO: {:02X}", opcode) }
            0x82 => { println!("TODO: {:02X}", opcode) }
            0x83 => { println!("TODO: {:02X}", opcode) }
            0x84 => { println!("TODO: {:02X}", opcode) }
            0x85 => { println!("TODO: {:02X}", opcode) }
            0x86 => { println!("TODO: {:02X}", opcode) }
            0x87 => { println!("TODO: {:02X}", opcode) }
            0x88 => { println!("TODO: {:02X}", opcode) }
            0x89 => { println!("TODO: {:02X}", opcode) }
            0x8A => { println!("TODO: {:02X}", opcode) }
            0x8B => { println!("TODO: {:02X}", opcode) }
            0x8C => { println!("TODO: {:02X}", opcode) }
            0x8D => { println!("TODO: {:02X}", opcode) }
            0x8E => { println!("TODO: {:02X}", opcode) }
            0x8F => { println!("TODO: {:02X}", opcode) }

            0x90 => { println!("TODO: {:02X}", opcode) }
            0x91 => { println!("TODO: {:02X}", opcode) }
            0x92 => { println!("TODO: {:02X}", opcode) }
            0x93 => { println!("TODO: {:02X}", opcode) }
            0x94 => { println!("TODO: {:02X}", opcode) }
            0x95 => { println!("TODO: {:02X}", opcode) }
            0x96 => { println!("TODO: {:02X}", opcode) }
            0x97 => { println!("TODO: {:02X}", opcode) }
            0x98 => { println!("TODO: {:02X}", opcode) }
            0x99 => { println!("TODO: {:02X}", opcode) }
            0x9A => { println!("TODO: {:02X}", opcode) }
            0x9B => { println!("TODO: {:02X}", opcode) }
            0x9C => { println!("TODO: {:02X}", opcode) }
            0x9D => { println!("TODO: {:02X}", opcode) }
            0x9E => { println!("TODO: {:02X}", opcode) }
            0x9F => { println!("TODO: {:02X}", opcode) }

            0xA0 => { println!("TODO: {:02X}", opcode) }
            0xA1 => { println!("TODO: {:02X}", opcode) }
            0xA2 => { println!("TODO: {:02X}", opcode) }
            0xA3 => { println!("TODO: {:02X}", opcode) }
            0xA4 => { println!("TODO: {:02X}", opcode) }
            0xA5 => { println!("TODO: {:02X}", opcode) }
            0xA6 => { println!("TODO: {:02X}", opcode) }
            0xA7 => { println!("TODO: {:02X}", opcode) }
            0xA8 => { println!("TODO: {:02X}", opcode) }
            0xA9 => { println!("TODO: {:02X}", opcode) }
            0xAA => { println!("TODO: {:02X}", opcode) }
            0xAB => { println!("TODO: {:02X}", opcode) }
            0xAC => { println!("TODO: {:02X}", opcode) }
            0xAD => { println!("TODO: {:02X}", opcode) }
            0xAE => { println!("TODO: {:02X}", opcode) }
            0xAF => { println!("TODO: {:02X}", opcode) }

            0xB0 => { println!("TODO: {:02X}", opcode) }
            0xB1 => { println!("TODO: {:02X}", opcode) }
            0xB2 => { println!("TODO: {:02X}", opcode) }
            0xB3 => { println!("TODO: {:02X}", opcode) }
            0xB4 => { println!("TODO: {:02X}", opcode) }
            0xB5 => { println!("TODO: {:02X}", opcode) }
            0xB6 => { println!("TODO: {:02X}", opcode) }
            0xB7 => { println!("TODO: {:02X}", opcode) }
            0xB8 => { println!("TODO: {:02X}", opcode) }
            0xB9 => { println!("TODO: {:02X}", opcode) }
            0xBA => { println!("TODO: {:02X}", opcode) }
            0xBB => { println!("TODO: {:02X}", opcode) }
            0xBC => { println!("TODO: {:02X}", opcode) }
            0xBD => { println!("TODO: {:02X}", opcode) }
            0xBE => { println!("TODO: {:02X}", opcode) }
            0xBF => { println!("TODO: {:02X}", opcode) }

            0xC0 => { println!("TODO: {:02X}", opcode) }
            0xC1 => { println!("TODO: {:02X}", opcode) }
            0xC2 => { println!("TODO: {:02X}", opcode) }
            0xC3 => self.reg_pc = self.ld_16(),
            0xC4 => { println!("TODO: {:02X}", opcode) }
            0xC5 => { println!("TODO: {:02X}", opcode) }
            0xC6 => { println!("TODO: {:02X}", opcode) }
            0xC7 => { println!("TODO: {:02X}", opcode) }
            0xC8 => { println!("TODO: {:02X}", opcode) }
            0xC9 => { println!("TODO: {:02X}", opcode) }
            0xCA => { println!("TODO: {:02X}", opcode) }
            0xCB => { println!("TODO: {:02X}", opcode) }
            0xCC => { println!("TODO: {:02X}", opcode) }
            0xCD => { println!("TODO: {:02X}", opcode) }
            0xCE => { println!("TODO: {:02X}", opcode) }
            0xCF => { println!("TODO: {:02X}", opcode) }

            0xD0 => { println!("TODO: {:02X}", opcode) }
            0xD1 => { println!("TODO: {:02X}", opcode) }
            0xD2 => { println!("TODO: {:02X}", opcode) }
            0xD3 => { println!("TODO: {:02X}", opcode) }
            0xD4 => { println!("TODO: {:02X}", opcode) }
            0xD5 => { println!("TODO: {:02X}", opcode) }
            0xD6 => { println!("TODO: {:02X}", opcode) }
            0xD7 => { println!("TODO: {:02X}", opcode) }
            0xD8 => { println!("TODO: {:02X}", opcode) }
            0xD9 => { println!("TODO: {:02X}", opcode) }
            0xDA => { println!("TODO: {:02X}", opcode) }
            0xDB => { println!("TODO: {:02X}", opcode) }
            0xDC => { println!("TODO: {:02X}", opcode) }
            0xDD => { println!("TODO: {:02X}", opcode) }
            0xDE => { println!("TODO: {:02X}", opcode) }
            0xDF => { println!("TODO: {:02X}", opcode) }

            0xE0 => { println!("TODO: {:02X}", opcode) }
            0xE1 => { println!("TODO: {:02X}", opcode) }
            0xE2 => { println!("TODO: {:02X}", opcode) }
            0xE3 => { println!("TODO: {:02X}", opcode) }
            0xE4 => { println!("TODO: {:02X}", opcode) }
            0xE5 => { println!("TODO: {:02X}", opcode) }
            0xE6 => { println!("TODO: {:02X}", opcode) }
            0xE7 => { println!("TODO: {:02X}", opcode) }
            0xE8 => { println!("TODO: {:02X}", opcode) }
            0xE9 => { println!("TODO: {:02X}", opcode) }
            0xEA => { println!("TODO: {:02X}", opcode) }
            0xEB => { println!("TODO: {:02X}", opcode) }
            0xEC => { println!("TODO: {:02X}", opcode) }
            0xED => { println!("TODO: {:02X}", opcode) }
            0xEE => { println!("TODO: {:02X}", opcode) }
            0xEF => { println!("TODO: {:02X}", opcode) }

            0xF0 => { println!("TODO: {:02X}", opcode) }
            0xF1 => { println!("TODO: {:02X}", opcode) }
            0xF2 => { println!("TODO: {:02X}", opcode) }
            0xF3 => { println!("TODO: {:02X}", opcode) }
            0xF4 => { println!("TODO: {:02X}", opcode) }
            0xF5 => { println!("TODO: {:02X}", opcode) }
            0xF6 => { println!("TODO: {:02X}", opcode) }
            0xF7 => { println!("TODO: {:02X}", opcode) }
            0xF8 => { println!("TODO: {:02X}", opcode) }
            0xF9 => { println!("TODO: {:02X}", opcode) }
            0xFA => { println!("TODO: {:02X}", opcode) }
            0xFB => { println!("TODO: {:02X}", opcode) }
            0xFC => { println!("TODO: {:02X}", opcode) }
            0xFD => { println!("TODO: {:02X}", opcode) }
            0xFE => self.cp(),
            0xFF => { println!("TODO: {:02X}", opcode) }

            _ => {
                println!("{:02X} Unimplemented", opcode);
            }
        };
    }

    fn nop(&mut self) {
        self.reg_pc.inc();
    }

    fn ld_16(&self) -> Reg16 {
        let low = self.rom[(self.reg_pc.get() + 1) as usize];
        let high = self.rom[(self.reg_pc.get() + 2) as usize];
        Reg16 { high: high, low: low }
    }

    fn cp(&mut self) {
        let a = self.reg_af.high;
        let n = self.rom[(self.reg_pc.get() + 1) as usize];
        self.set_zero_flag(a == n);
        self.set_sub_flag(true);
        self.set_carry_flag(a < n);
        self.set_half_carry_flag(Cpu::get_sub_half_carry(a, n));
        self.reg_pc.inc();
    }

    fn get_add_half_carry(first: u8, second: u8) -> bool {
       return ((first & 0xf) + (second & 0xf)) & 0x10 == 0x10;
    }

    fn get_sub_half_carry(first: u8, second: u8) -> bool {
       return (first & 0xf) < (second & 0xf);
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::Memory;

    fn test_setup() -> Cpu {
        let test_rom = Box::new([0x00, 0x01, 0xFF, 0xC3, 0x33, 0x6A]);
        let test_mem = Memory::new();
        let mut test_cpu = Cpu::new(test_rom, test_mem);
        test_cpu.reg_pc.set(0);
        test_cpu
    }

    #[test]
    fn test_nop() {
        let mut test_cpu = test_setup();
        let pc_before = test_cpu.reg_pc.get();
        let cycles = BASE_CYCLES[0x00 as usize];
        test_cpu.nop();
        let pc_after = test_cpu.reg_pc.get();
        assert!(cycles == 4);
        assert!(pc_before + 1 == pc_after);
    }

    #[test]
    fn test_jp() {
        let mut test_cpu = test_setup();
        let cycles = BASE_CYCLES[0xC3 as usize];
        test_cpu.reg_pc = test_cpu.ld_16();
        assert!(cycles == 16);
        assert!(test_cpu.reg_pc.get() == 0xFF01);
    }
}
