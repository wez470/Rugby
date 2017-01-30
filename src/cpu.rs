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

pub enum Regs {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    SP,
    PC,
}

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

        let mut cycles = BASE_CYCLES[opcode as usize];

        match opcode {
            0x00 => self.nop(),
            0x11 => self.reg_de = self.ld_16(),
            0x28 => self.jr_z_signed_8(&mut cycles),
            0xAF => self.xor(Regs::A),
            0xC3 => self.reg_pc = self.ld_16(),
            0xFE => self.cp(),

            _ => {
                println!("{:02X} Unimplemented", opcode);
            }
        };
    }

    fn nop(&mut self) {
        self.reg_pc.inc();
    }

    fn ld_16(&mut self) -> Reg16 {
        let low = self.rom[(self.reg_pc.get() + 1) as usize];
        let high = self.rom[(self.reg_pc.get() + 2) as usize];
        let new_pc = self.reg_pc.get() + 3;
        self.reg_pc.set(new_pc);
        Reg16 { high: high, low: low }
    }

    fn jr_z_signed_8(&mut self, cycles: &mut i32) {
        if self.get_zero_flag() {
            *cycles += 4;
            let val = self.rom[(self.reg_pc.get() + 1) as usize] as i8;
            let new_pc = (self.reg_pc.get() as i32 + val as i32) as u16;
            self.reg_pc.set(new_pc);
        }
        else {
            let new_pc = self.reg_pc.get() + 2;
            self.reg_pc.set(new_pc);
        }
    }

    fn xor(&mut self, reg: Regs) {
        match reg {
            Regs::A => { println!("Got here") }
            Regs::F => { println!("placeholder") }
            Regs::B => { println!("placeholder") }
            Regs::C => { println!("placeholder") }
            Regs::D => { println!("placeholder") }
            Regs::E => { println!("placeholder") }
            Regs::H => { println!("placeholder") }
            Regs::L => { println!("placeholder") }
            Regs::SP => { println!("placeholder") }
            Regs::PC => { println!("placeholder") }
        }
    }

    fn cp(&mut self) {
        let a = self.reg_af.high;
        let n = self.rom[(self.reg_pc.get() + 1) as usize];
        self.set_zero_flag(a == n);
        self.set_sub_flag(true);
        self.set_carry_flag(a < n);
        self.set_half_carry_flag(Cpu::get_sub_half_carry(a, n));
        let new_pc = self.reg_pc.get() + 2;
        self.reg_pc.set(new_pc);
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
