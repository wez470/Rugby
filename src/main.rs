struct sixteen_bit_reg {
    high: u8,
    low: u8
}

impl sixteen_bit_reg {
    fn new() -> sixteen_bit_reg {
        sixteen_bit_reg {
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

struct CPU {
    reg_af: sixteen_bit_reg,
    reg_bc: sixteen_bit_reg,
    reg_de: sixteen_bit_reg,
    reg_hl: sixteen_bit_reg,
    reg_sp: sixteen_bit_reg,
    reg_pc: sixteen_bit_reg
}

impl CPU {
    fn new() -> CPU {
        CPU {
            reg_af: sixteen_bit_reg::new(),
            reg_bc: sixteen_bit_reg::new(),
            reg_de: sixteen_bit_reg::new(),
            reg_hl: sixteen_bit_reg::new(),
            reg_sp: sixteen_bit_reg::new(),
            reg_pc: sixteen_bit_reg::new()
        }
    }
}

fn main() {
    let mut cpu = CPU::new();

    let mut test_reg = sixteen_bit_reg::new();
    test_reg.set(60000);
    println!("Value in test reg: {}", test_reg.get());
    test_reg.inc();
    println!("Value in test reg: {}", test_reg.get());
}
