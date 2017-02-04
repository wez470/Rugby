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

    pub fn set_bit_8(&mut self, set: bool) {
        if set {
            self.low |= 128;
        }
        else {
            self.low &= 127;
        }
    }

    pub fn is_bit_8_set(&self) -> bool {
        self.low & 128 == 128
    }

    pub fn set_bit_7(&mut self, set: bool) {
        if set {
            self.low |= 64;
        }
        else {
            self.low &= 191;
        }
    }

    pub fn set_bit_6(&mut self, set: bool) {
        if set {
            self.low |= 32;
        }
        else {
            self.low &= 223;
        }
    }

    pub fn set_bit_5(&mut self, set: bool) {
        if set {
            self.low |= 16;
        }
        else {
            self.low &= 239;
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
