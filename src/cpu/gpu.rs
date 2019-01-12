const GPU_MODE_CYCLES: usize = 456; // Number of CPU cycles to cycle through LCD modes
const VIDEO_RAM_SIZE: usize = 8 * 1024; // 8 KB

#[derive(Clone)]
pub enum Mode {
    HorizontalBlank = 0,
    VerticalBlank = 1,
    OamRead = 2,
    VRamRead = 3,
}

#[derive(Clone)]
pub struct Gpu {
    /// Video RAM internal to the Gameboy.
    pub video_ram: Box<[u8]>,

    /// Where we are at currently in the lcd cycle counter
    cycle_counter: usize,

    /// The current scanline. (LY at address 0xFF44)
    scan_line: u8,

    /// True if the display is enabled
    lcd_enabled: bool,

    /// The current LCD mode
    mode: Mode,

    /// True if the window is enabled
    window_enabled: bool,

    /// True if the background is enabled
    background_enabled: bool,

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
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            video_ram: Box::new([0; VIDEO_RAM_SIZE]),
            cycle_counter: 0,
            scan_line: 0,
            lcd_enabled: true,
            mode: Mode::HorizontalBlank,
            window_enabled: false,
            background_enabled: false,
            coincidence_flag: false,
            coincidence_interrupt: false,
            oam_enabled: false,
            vertical_blank_interrupt: false,
            horizontal_blank_interrupt: false,
            scan_x: 0,
            scan_y: 0,
            window_x: 0,
            window_y: 0,
        }
    }

    pub fn step(&mut self, cycles: usize) {
        if !self.lcd_enabled {
            return
        }
        self.cycle_counter += cycles;

        if self.cycle_counter >= GPU_MODE_CYCLES {
            self.scan_line += 1;
            if self.scan_line > 153 {
                self.scan_line = 0;
                self.mode = Mode::OamRead;
            }

            self.cycle_counter %= GPU_MODE_CYCLES;
        }
    }
}