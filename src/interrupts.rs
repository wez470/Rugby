/// Represents bit indexes of the interrupts
#[derive(Clone, Copy, Debug)]
pub enum Interrupt {
    VerticalBlank = 0,
    LCD = 1,
    Timer = 2,
    Serial = 3,
    Joypad = 4,
}
