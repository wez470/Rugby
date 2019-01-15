const HORIZONTAL_BLANK_CYCLES: usize = 204; // Horizontal blank phase takes 201-207 cycles.
const OAM_READ_CYCLES: usize = 80; // OAM read phase takes 77-83 cycles.
const VRAM_READ_CYCLES: usize = 172; // VRAM read phase takes 169-175 cycles.
const SCAN_LINE_CYCLES: usize = 456; // One scan line takes 456 cycles.
const VERTICAL_BLANK_START_LINE: u8 = 144; // The scan line at which we enter the vertical blank phase
const VERTICAL_BLANK_END_LINE: u8 = 154; // The scan line at which the vertical blank phase ends
const VIDEO_RAM_SIZE: usize = 8 * 1024; // 8 KB

#[derive(Clone, Copy)]
pub enum Mode {
    HorizontalBlank = 0,
    VerticalBlank = 1,
    OamRead = 2,
    VRamRead = 3,
}

#[derive(Clone, Copy)]
pub enum TileMapLocation {
    X9800 = 0,
    X9C00 = 1,
}

#[derive(Clone, Copy)]
pub enum BackgroundAndWindowTileDataLocation {
    X8800 = 0,
    X8000 = 1,
}

#[derive(Clone)]
pub struct Gpu {
    /// Video RAM internal to the Gameboy.
    pub video_ram: Box<[u8]>,

    /// The current scan line. (LY at address 0xFF44)
    pub scan_line: u8,

    /// Where we are at currently in the lcd cycle counter
    cycles: usize,

    /// True if the display is enabled
    lcd_enabled: bool,

    /// The current LCD mode
    pub mode: Mode,

    /// Setting to False will clear the background
    background_display: bool,

    /// True if the window is enabled
    window_enabled: bool,

    /// True if sprite display is enabled
    obj_display_enabled: bool,

    /// The Coincidence flag (Bit 2 in 0xFF41)
    /// TODO(wcarlson): Add more documentation about this
    coincidence_flag: bool,

    /// True if the coincidence interrupt is enabled (Bit 6 in 0xFF41)
    coincidence_interrupt: bool,

    /// True if OAM interrupt is enabled. (Bit 5 in 0xFF41)
    oam_enabled: bool,

    /// True if the Vertical Blank interrupt is enabled. (Bit 4 in 0xFF41)
    vertical_blank_interrupt: bool,

    /// True if the Horizontal Blank interrupt is enabled. (Bit 3 in 0xFF41)
    horizontal_blank_interrupt: bool,

    /// X and Y positions in the 256x256 pixel background map to start at the top left
    /// of the LCD screen.
    scan_x: u8,
    scan_y: u8,

    /// Window X and Y positions
    window_x: u8,
    window_y: u8,

    /// The address which the window tile map starts
    window_tile_map: TileMapLocation,

    /// The address which the background and window tile data start
    background_and_window_tile_data_location: BackgroundAndWindowTileDataLocation,

    /// The address which the background tile map starts
    background_tile_map: TileMapLocation,
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            video_ram: Box::new([0; VIDEO_RAM_SIZE]),
            cycles: 0,
            scan_line: 0,
            lcd_enabled: true,
            mode: Mode::OamRead,
            window_enabled: false,
            obj_display_enabled: false,
            background_display: false,
            coincidence_flag: false,
            coincidence_interrupt: false,
            oam_enabled: false,
            vertical_blank_interrupt: false,
            horizontal_blank_interrupt: false,
            scan_x: 0,
            scan_y: 0,
            window_x: 0,
            window_y: 0,
            window_tile_map: TileMapLocation::X9800,
            background_and_window_tile_data_location: BackgroundAndWindowTileDataLocation::X8800,
            background_tile_map: TileMapLocation::X9800,
        }
    }

    /// Step through the LCD phases
    ///
    /// Each scan line takes `SCAN_LINE_CYCLES` to complete. For lines 0-143 we go from
    /// `Mode::OamRead` -> `Mode::VRamRead` -> `Mode::HorizontalBlank` on each line.
    /// For lines 144-153, we stay in `Mode::VerticalBlank` for the whole line, after which
    /// we go back to line 0
    pub fn step(&mut self, cycles: usize) {
        if !self.lcd_enabled {
            return
        }
        self.cycles += cycles;

        match self.mode {
            Mode::HorizontalBlank => {
                println!("HORIZONTAL BLANK");
                if self.cycles >= HORIZONTAL_BLANK_CYCLES {
                    self.cycles %= HORIZONTAL_BLANK_CYCLES;
                    self.scan_line += 1;

                    if self.scan_line >= VERTICAL_BLANK_START_LINE {
                        self.mode = Mode::VerticalBlank;
                    } else {
                        self.mode = Mode::OamRead;
                    }
                }
            },
            Mode::VerticalBlank => {
                println!("VERTICAL BLANK");
                if self.cycles > SCAN_LINE_CYCLES {
                    self.cycles %= SCAN_LINE_CYCLES;
                    self.scan_line += 1;

                    if self.scan_line >= VERTICAL_BLANK_END_LINE {
                        self.scan_line = 0;
                        self.mode = Mode::OamRead;
                    }
                }
            },
            Mode::OamRead => {
                println!("OAM READ");
                if self.cycles > OAM_READ_CYCLES {
                    self.cycles %= OAM_READ_CYCLES;
                    self.mode = Mode::VRamRead;
                }
            },
            Mode::VRamRead => {
                println!("VRAM READ");
                if self.cycles > VRAM_READ_CYCLES {
                    self.cycles %= VRAM_READ_CYCLES;
                    self.mode = Mode::HorizontalBlank;
                    // TODO(wcarlson): Render scan line
                }
            },
        }
    }

    pub fn read_lcd_control(&self) -> u8 {
        let mut lcd_control = 0;
        if self.lcd_enabled {
            lcd_control |= 1 << 7;
        }
        lcd_control |= (self.window_tile_map as u8) << 6;
        if self.window_enabled {
            lcd_control |= 1 << 5;
        }
        lcd_control |= (self.background_and_window_tile_data_location as u8) << 4;
        lcd_control |= (self.background_tile_map as u8) << 3;
        //TODO(wcarlson): Bit 2 - OBJ (Sprite) Size

        if self.obj_display_enabled {
            lcd_control |= 1 << 1
        }
        if self.background_display {
            lcd_control |= 1
        }
        lcd_control
    }

    pub fn write_lcd_control(&mut self, val: u8) {
        self.lcd_enabled = (val >> 7) == 1;

        //TODO(wcarlson): Bit 6 - Window Tile Map Display Select

        self.window_enabled = ((val >> 5) & 1) == 1;

        //TODO(wcarlson): Bit 4 - BG & Window Tile Data Select
        //TODO(wcarlson): Bit 3 - BG Tile Map Display Select
        //TODO(wcarlson): Bit 2 - OBJ (Sprite) Size

        self.obj_display_enabled = ((val >> 1) & 1) == 1;
        self.background_display = (val & 1) == 1;
    }
}