/// Represents bit indexes of the interrupts
#[derive(Clone, Copy, Debug)]
pub enum Interrupt {
    VerticalBlank = 0,
    LCD = 1,
    Timer = 2,
    Serial = 3,
    Joypad = 4,
}

impl Interrupt {
    /// Get the memory address of the code which handles this interrupt.
    pub fn handler_addr(self) -> u16 {
        0x0040 + 8 * (self as u16)
    }
}