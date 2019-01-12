const GPU_MODE_CYCLES: usize = 456; // Number of CPU cycles to cycle through LCD modes
const VIDEO_RAM_SIZE: usize = 8 * 1024; // 8 KB

#[derive(Clone)]
pub struct Gpu {
    /// Video RAM internal to the Gameboy.
    pub video_ram: Box<[u8]>,

    /// Where we are at currently in the scanline cycle counter
    cycle_counter: usize,

    /// The current scanline. (LY at address 0xFF44)
    scan_line: usize,

    /// If the display is enabled or not
    lcd_enabled: bool,
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            video_ram: Box::new([0; VIDEO_RAM_SIZE]),
            cycle_counter: 0,
            scan_line: 0,
            lcd_enabled: true,
        }
    }

    pub fn step(&mut self, cycles: usize) {
        if !self.lcd_enabled {
            return
        }
        self.cycle_counter += cycles;

        if self.cycle_counter >= GPU_MODE_CYCLES {
            self.scan_line += 1;

            self.cycle_counter %= GPU_MODE_CYCLES;
        }
    }
}