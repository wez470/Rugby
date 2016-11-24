pub struct Reg16 {
    pub high: u8,
    pub low: u8
}

impl Reg16 {
    pub fn new() -> Reg16 {
        Reg16 {
            high: 0,
            low: 0
        }
    }

    pub fn set(&mut self, val: u16) {
        self.low = (val & 0xFF) as u8;
        self.high = ((val >> 8) & 0xFF) as u8;
    }

    pub fn get(&self) -> u16 {
        ((self.high as u16) << 8) | (self.low as u16)
    }

    pub fn inc(&mut self) {
        let val: u16 = self.get();
        self.set(val + 1);
    }
}
