#[derive(Clone, Default)]
pub struct Reg16 {
    pub high: u8,
    pub low: u8
}

impl Reg16 {
    pub fn set(&mut self, val: u16) {
        self.low = (val & 0xFF) as u8;
        self.high = ((val >> 8) & 0xFF) as u8;
    }

    pub fn get(&self) -> u16 {
        ((self.high as u16) << 8) | (self.low as u16)
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

#[test]
fn test_set_16_bit_reg() {
    let mut reg = Reg16::default();
    reg.set(60000);
    assert_eq!(reg.low, 96);
    assert_eq!(reg.high, 234);
}

#[test]
fn test_get_16_bit_reg() {
    let mut reg = Reg16::default();
    reg.low = 96;
    reg.high = 234;
    assert_eq!(reg.get(), 60000);
}

#[test]
fn test_inc() {
    let mut reg = Reg16::default();
    reg.set(0);
    reg.inc(1);
    assert_eq!(reg.get(), 1);
}
