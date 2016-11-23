struct Reg16 {
    high: u8,
    low: u8
}

impl Reg16 {
    fn new() -> Reg16 {
        Reg16 {
            high: 0,
            low: 0
        }
    }

    fn set_low(&mut self, val: u8) {
        self.low = val;
    }

    fn get_low(&self) -> u8 {
        self.low
    }

    fn inc_low(&mut self) {
        self.low += 1;
    }

    fn set_high(&mut self, val: u8) {
        self.high = val;
    }

    fn get_high(&self) -> u8 {
        self.high
    }

    fn inc_high(&mut self) {
        self.high += 1;
    }

    fn set(&mut self, val: u16) {
        self.low = (val & 0xFF) as u8;
        self.high = ((val >> 8) & 0xFF) as u8;
    }

    fn get(&self) -> u16 {
        ((self.high as u16) << 8) | (self.low as u16)
    }

    fn inc(&mut self) {
        let val: u16 = self.get();
        self.set(val + 1);
    }

}

struct Cpu {
    reg_af: Reg16,
    reg_bc: Reg16,
    reg_de: Reg16,
    reg_hl: Reg16,
    reg_sp: Reg16,
    reg_pc: Reg16
}

impl Cpu {
    fn new() -> Cpu {
        Cpu {
            reg_af: Reg16::new(),
            reg_bc: Reg16::new(),
            reg_de: Reg16::new(),
            reg_hl: Reg16::new(),
            reg_sp: Reg16::new(),
            reg_pc: Reg16::new()
        }
    }
}

fn main() {
    let mut cpu = Cpu::new();

    let mut test_reg = Reg16::new();
    test_reg.set(60000);
    println!("Value in test reg: {}", test_reg.get());
    test_reg.inc();
    println!("Value in test reg: {}", test_reg.get());
}
