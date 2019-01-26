#[derive(Clone, Copy)]
pub struct Sprite {
    /// Sprite x and y positions
    pub x: u8,
    pub y: u8,

    /// The tile number of the sprite
    pub tile_num: u8,

    /// True if the sprite is above the background
    above_background: bool,

    /// True if the sprite should be flipped
    flip_x: bool,
    flip_y: bool,

    /// Palette number of the sprite
    palette_num: u8,
}

impl Sprite {
    pub fn new() -> Sprite {
        Sprite {
            x: 0,
            y: 0,
            tile_num: 0,
            above_background: true,
            flip_x: false,
            flip_y: false,
            palette_num: 0,
        }
    }

    /// Write sprite byte 3. bits 0-3 are for CGB only
    pub fn write_attributes(&mut self, val: u8) {
        self.above_background = ((val >> 7) & 1) == 0;
        self.flip_y = ((val >> 6) & 1) == 1;
        self.flip_x = ((val >> 5) & 1) == 1;
        self.palette_num = (val >> 4) & 1;
    }
}