use enumflags2::BitFlags;
use std::collections::BinaryHeap;
use crate::interrupts::Interrupt;

mod sprite;

const HORIZONTAL_BLANK_CYCLES: usize = 204; // Horizontal blank phase takes 201-207 cycles.
const OAM_READ_CYCLES: usize = 80; // OAM read phase takes 77-83 cycles.
const VRAM_READ_CYCLES: usize = 172; // VRAM read phase takes 169-175 cycles.
const SCAN_LINE_CYCLES: usize = 456; // One scan line takes 456 cycles.
const VERTICAL_BLANK_START_LINE: u8 = 144; // The scan line at which we enter the vertical blank phase
const VERTICAL_BLANK_END_LINE: u8 = 154; // The scan line at which the vertical blank phase ends
const VIDEO_RAM_SIZE: usize = 8 * 1024; // 8 KB
const TOTAL_TILES: usize = 384; // Total number of tiles in video ram
const TILE_MAP_0_START: usize = 0x1800; // The starting address of tile map 0
const SPRITE_RAM_SIZE: usize = 160; // For the address range 0xFE00-0xFE9F (inclusive).
const TOTAL_SPRITES: usize = 40; // The number of sprites in sprite ram
const BYTES_PER_SPRITE: usize = 4;
pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;

#[derive(Clone, Copy)]
enum Mode {
    HorizontalBlank = 0,
    VerticalBlank = 1,
    OamRead = 2,
    VRamRead = 3,
}

#[derive(Clone, Copy)]
enum TileMapLocation {
    X9800 = 0,
    X9C00 = 1,
}

impl std::convert::From<u8> for TileMapLocation {
    fn from(value: u8) -> TileMapLocation {
        match value {
            0 => TileMapLocation::X9800,
            1 => TileMapLocation::X9C00,
            _ => panic!("Invalid number for TileMapLocation"),
        }
    }
}

#[derive(Clone, Copy)]
enum BackgroundAndWindowLocation {
    X8800 = 0,
    X8000 = 1,
}

impl std::convert::From<u8> for BackgroundAndWindowLocation {
    fn from(value: u8) -> BackgroundAndWindowLocation {
        match value {
            0 => BackgroundAndWindowLocation::X8800,
            1 => BackgroundAndWindowLocation::X8000,
            _ => panic!("Invalid number for BackgroundAndWindowTileDataLocation"),
        }
    }
}

#[derive(Clone, Copy)]
enum ObjSize {
    EightByEight = 0,
    EightBySixteen = 1,
}

impl std::convert::From<u8> for ObjSize {
    fn from(value: u8) -> ObjSize {
        match value {
            0 => ObjSize::EightByEight,
            1 => ObjSize::EightBySixteen,
            _ => panic!("Invalid number for ObjSize"),
        }
    }
}

type Tile = [[u8; 8]; 8];

fn init_tile() -> Tile {
    [[0; 8]; 8]
}

#[derive(Clone)]
pub struct Gpu {
    /// Current screen
    pub screen_buffer: Box<[[u8; SCREEN_WIDTH]; SCREEN_HEIGHT]>,

    /// The current background
    background: Box<[[u8; 256]; 256]>,

    /// The current window
    window: Box<[[u8; 256]; 256]>,

    /// Video RAM internal to the Gameboy.
    video_ram: Box<[u8]>,

    /// The current tiles in video ram.
    tile_set: Box<[Tile]>,

    /// Sprite RAM internal to the Game Boy, also known as OAM.
    sprite_ram: Box<[u8]>,

    /// The current sprites in sprite ram
    sprites: Box<[sprite::Sprite]>,

    /// The current scan line. (LY at 0xFF44)
    scan_line: u8,

    /// The scan line compare register. (LYC at 0xFF45). An interrupt will occur (if enabled) if
    /// the scan line equals the scan line compare
    scan_line_compare: u8,

    /// Where we are at currently in the lcd cycle counter
    cycles: usize,

    /// True if the display is enabled
    lcd_enabled: bool,

    /// The current LCD mode
    mode: Mode,

    /// Setting to False will clear the background
    background_enabled: bool,

    /// True if the window is enabled
    window_enabled: bool,

    /// True if sprite display is enabled
    obj_display_enabled: bool,

    /// True if the coincidence interrupt is enabled (Bit 6 in 0xFF41)
    coincidence_interrupt: bool,

    /// True if OAM interrupt is enabled. (Bit 5 in 0xFF41)
    oam_interrupt: bool,

    /// True if the Vertical Blank interrupt is enabled. (Bit 4 in 0xFF41)
    vertical_blank_interrupt: bool,

    /// True if the Horizontal Blank interrupt is enabled. (Bit 3 in 0xFF41)
    horizontal_blank_interrupt: bool,

    /// X and Y positions in the 256x256 pixel background map to start at the top left of the LCD
    /// screen.
    scan_x: u8,
    scan_y: u8,

    /// Window X and Y positions
    window_x: u8,
    window_y: u8,

    /// The address which the window tile map starts
    window_tile_map: TileMapLocation,

    /// The address which the background and window tile data start
    background_and_window_location: BackgroundAndWindowLocation,

    /// The address which the background tile map starts
    background_tile_map: TileMapLocation,

    /// The size of the objects (sprites). 8x8 or 8x16
    obj_size: ObjSize,

    /// Background palette register
    background_palette: u8,

    /// First sprite palette register
    obj_palette_0: u8,

    /// Second sprite palette register
    obj_palette_1: u8,
}

impl Gpu {
    pub fn new() -> Gpu {
        let mut gpu = Gpu {
            // TODO(solson): Figure out a clean way to allocate 2D arrays like these directly on
            // the heap (without giving up the `arr[i][j]` multidimensional indexing).
            screen_buffer: Box::new([[0u8; SCREEN_WIDTH]; SCREEN_HEIGHT]),
            background: Box::new([[0u8; 256]; 256]),
            window: Box::new([[0u8; 256]; 256]),
            video_ram: vec![0; VIDEO_RAM_SIZE].into_boxed_slice(),
            tile_set: vec![init_tile(); TOTAL_TILES].into_boxed_slice(),
            sprite_ram: vec![0; SPRITE_RAM_SIZE].into_boxed_slice(),
            sprites: vec![sprite::Sprite::new(); TOTAL_SPRITES].into_boxed_slice(),
            cycles: 0,
            scan_line: 0,
            scan_line_compare: 0,
            lcd_enabled: true,
            mode: Mode::OamRead,
            window_enabled: false,
            obj_display_enabled: false,
            background_enabled: true,
            coincidence_interrupt: false,
            oam_interrupt: false,
            vertical_blank_interrupt: false,
            horizontal_blank_interrupt: false,
            scan_x: 0,
            scan_y: 0,
            window_x: 0,
            window_y: 0,
            window_tile_map: TileMapLocation::X9800,
            background_and_window_location: BackgroundAndWindowLocation::X8000,
            background_tile_map: TileMapLocation::X9800,
            obj_size: ObjSize::EightByEight,
            background_palette: 0,
            obj_palette_0: 0,
            obj_palette_1: 1,
        };
        for i in 0..TOTAL_SPRITES {
            gpu.sprites[i].index = i;
        }
        gpu
    }

    pub fn read_sprite_ram(&self, addr: usize) -> u8 {
        self.sprite_ram[addr]
    }

    pub fn write_sprite_ram(&mut self, addr: usize, val: u8) {
        self.sprite_ram[addr] = val;

        let sprite_index = addr / BYTES_PER_SPRITE;
        let sprite_byte = addr % BYTES_PER_SPRITE;
        match sprite_byte {
            0 => self.sprites[sprite_index].y = val.wrapping_sub(16),
            1 => self.sprites[sprite_index].x = val.wrapping_sub(8),
            2 => self.sprites[sprite_index].tile_num = val,
            3 => self.sprites[sprite_index].write_attributes(val),
            _ => panic!("Invalid sprite byte index"),
        }

    }

    pub fn read_vram(&self, addr: usize) -> u8 {
        self.video_ram[addr]
    }

    /// Write video ram
    ///
    /// This function also keeps the current tile set up to date
    pub fn write_vram(&mut self, addr: usize, val: u8) {
        self.video_ram[addr] = val;
        if addr >= TILE_MAP_0_START {
            return
        }

        // Each row in the tile (8x8 pixels) is 2 bytes
        let row_start = addr & 0xFFFE;
        let byte1 = self.video_ram[row_start];
        let byte2 = self.video_ram[row_start + 1];

        // Each tile is 16 byte total
        let tile_index = addr / 16;
        // Each row in a tile is 2 bytes
        let tile_row_index = (addr % 16) / 2;

        for pixel in 0..8 {
            let msb = byte2 >> (7 - pixel) & 1;
            let lsb = byte1 >> (7 - pixel) & 1;
            let new_pixel = (msb << 1) | lsb;

            self.tile_set[tile_index][tile_row_index][pixel] = new_pixel;
        }
    }

    /// Step through the LCD phases
    ///
    /// Each scan line takes `SCAN_LINE_CYCLES` to complete. For lines 0-143 we go from
    /// `Mode::OamRead` -> `Mode::VRamRead` -> `Mode::HorizontalBlank` on each line.
    /// For lines 144-153, we stay in `Mode::VerticalBlank` for the whole line, after which
    /// we go back to line 0
    pub fn step(&mut self, cycles: usize) -> BitFlags<Interrupt> {
        let mut interrupts = BitFlags::empty();
        if !self.lcd_enabled { return interrupts; }
        self.cycles += cycles;

        match self.mode {
            Mode::HorizontalBlank => {
                if self.cycles >= HORIZONTAL_BLANK_CYCLES {
                    self.cycles %= HORIZONTAL_BLANK_CYCLES;
                    self.scan_line += 1;

                    if self.scan_line >= VERTICAL_BLANK_START_LINE {
                        self.mode = Mode::VerticalBlank;
                        if self.vertical_blank_interrupt {
                            interrupts.insert(Interrupt::Lcd)
                        }
                        interrupts.insert(Interrupt::VBlank);
                    } else {
                        self.mode = Mode::OamRead;
                    }

                    if self.coincidence_interrupt && self.scan_line == self.scan_line_compare {
                        interrupts.insert(Interrupt::Lcd)
                    }
                }
            }

            Mode::VerticalBlank => {
                if self.cycles > SCAN_LINE_CYCLES {
                    self.cycles %= SCAN_LINE_CYCLES;
                    self.scan_line += 1;

                    if self.scan_line >= VERTICAL_BLANK_END_LINE {
                        self.scan_line = 0;
                        self.mode = Mode::OamRead;
                        if self.oam_interrupt {
                            interrupts.insert(Interrupt::Lcd);
                        }
                    }

                    if self.coincidence_interrupt && self.scan_line == self.scan_line_compare {
                        interrupts.insert(Interrupt::Lcd)
                    }
                }
            }

            Mode::OamRead => {
                if self.cycles > OAM_READ_CYCLES {
                    self.cycles %= OAM_READ_CYCLES;
                    self.mode = Mode::VRamRead;
                }
            }

            Mode::VRamRead => {
                if self.cycles > VRAM_READ_CYCLES {
                    self.cycles %= VRAM_READ_CYCLES;
                    self.mode = Mode::HorizontalBlank;
                    self.render_scan_line();

                    if self.horizontal_blank_interrupt {
                        interrupts.insert(Interrupt::Lcd)
                    }
                }
            }
        }

        interrupts
    }

    fn render_scan_line(&mut self) {
        if self.background_enabled {
            self.render_background_line();
        }
        if self.window_enabled {
            self.render_window_line();
        }
        if self.obj_display_enabled {
            self.render_sprite_line();
        }
    }

    fn render_background_line(&mut self) {
        let background_map = match self.background_tile_map {
            TileMapLocation::X9800 => &self.video_ram[0x1800..0x1C00],
            TileMapLocation::X9C00 => &self.video_ram[0x1C00..0x2000],
        };

        for i in 0..0x400 {
            let curr_tile_index: u8 = background_map[i];
            let curr_tile = match self.background_and_window_location {
                BackgroundAndWindowLocation::X8000 => self.tile_set[curr_tile_index as usize],
                BackgroundAndWindowLocation::X8800 => self.tile_set[(256 + ((curr_tile_index as i8) as i16)) as usize]
            };

            let curr_tile_row = i / 32;
            let curr_tile_col = i % 32;

            for r in 0..8 {
                for c in 0..8 {
                    self.background[curr_tile_row * 8 + r][curr_tile_col * 8 + c] = curr_tile[r][c];
                }
            }
        }

        for i in 0..SCREEN_WIDTH {
            let pixel_x = (self.scan_x as usize + i) % 256;
            let pixel_y = (self.scan_line as usize + self.scan_y as usize) % 256;
            let color = get_palette_color(self.background[pixel_y][pixel_x], self.background_palette);
            self.screen_buffer[self.scan_line as usize][i] = color;
        }
    }

    fn render_window_line(&mut self) {
        let window_map = match self.window_tile_map {
            TileMapLocation::X9800 => &self.video_ram[0x1800..0x1C00],
            TileMapLocation::X9C00 => &self.video_ram[0x1C00..0x2000],
        };

        for i in 0..0x400 {
            let curr_tile_index: u8 = window_map[i];
            let curr_tile = match self.background_and_window_location {
                BackgroundAndWindowLocation::X8000 => self.tile_set[curr_tile_index as usize],
                BackgroundAndWindowLocation::X8800 => self.tile_set[(256 + ((curr_tile_index as i8) as i16)) as usize]
            };

            let curr_tile_row = i / 32;
            let curr_tile_col = i % 32;

            for r in 0..8 {
                for c in 0..8 {
                    self.window[curr_tile_row * 8 + r][curr_tile_col * 8 + c] = curr_tile[r][c];
                }
            }
        }

        for screen_x in (self.window_x as usize)..SCREEN_WIDTH {
            let (y, overflow) = self.scan_line.overflowing_sub(self.window_y);
            if !overflow && (y as usize) < SCREEN_HEIGHT {
                let color = get_palette_color(self.window[y as usize][screen_x - (self.window_x as usize)], self.background_palette);
                self.screen_buffer[self.scan_line as usize][screen_x] = color;
            }
        }
    }

    fn render_sprite_line(&mut self) {
        let height = match self.obj_size {
            ObjSize::EightBySixteen => 16,
            ObjSize::EightByEight => 8,
        };

        let mut heap = BinaryHeap::new();
        for i in 0..TOTAL_SPRITES {
            let s = self.sprites[i];
            // Sprites at y = 0 or y >= 160 are off screen
            if self.scan_line.wrapping_sub(s.y) < height {
                heap.push(s);
            }
        }
        let mut sprites_to_render = heap.into_sorted_vec();
        sprites_to_render.truncate(10);
        for s in sprites_to_render.iter().rev() {
            let mut tile_num = s.tile_num as usize;
            let mut line = if s.flip_y {
                height - self.scan_line.wrapping_sub(s.y) - 1
            } else {
                self.scan_line.wrapping_sub(s.y)
            };

            if line >= 8 {
                tile_num += 1;
                line -= 8;
            }

            let tile =  self.tile_set[tile_num];

            for x in (0..8).rev() {
                let tile_x = if s.flip_x {
                    7 - x
                } else {
                    x
                } as usize;
                let target_x = s.x.wrapping_add(x);
                if target_x < SCREEN_WIDTH as u8 {
                    if tile[line as usize][tile_x] > 0
                        && (s.above_background || (!s.above_background && self.screen_buffer[self.scan_line as usize][target_x as usize] == 0)) {
                        let palette = if s.palette_num == 0 {
                            self.obj_palette_0
                        } else {
                            self.obj_palette_1
                        };
                        let color = get_palette_color(tile[line as usize][tile_x], palette);
                        self.screen_buffer[self.scan_line as usize][target_x as usize] = color;
                    }
                }
            }
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x40 => self.read_lcd_control(),
            0x41 => self.read_lcd_stat(),
            0x42 => self.scan_y,
            0x43 => self.scan_x,
            0x44 => self.scan_line,
            0x45 => self.scan_line_compare,
            // 0x46 is handled in the CPU.
            0x47 => self.background_palette,
            0x48 => self.obj_palette_0,
            0x49 => self.obj_palette_1,
            0x4A => self.window_y,
            0x4B => self.window_x.wrapping_add(7),
            _ => panic!("Invalid read address for GPU"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x40 => self.write_lcd_control(val),
            0x41 => self.write_lcd_stat(val),
            0x42 => self.scan_y = val,
            0x43 => self.scan_x = val,
            0x44 => self.scan_line = 0,
            0x45 => self.scan_line_compare = val,
            // 0x46 is handled in the CPU.
            0x47 => self.background_palette = val,
            0x48 => self.obj_palette_0 = val,
            0x49 => self.obj_palette_1 = val,
            0x4A => self.window_y = val,
            0x4B => self.window_x = val.wrapping_sub(7),
            _ => panic!("Invalid write address for GPU"),
        }
    }

    fn read_lcd_control(&self) -> u8 {
        let mut lcd_control = 0;
        lcd_control |= (self.lcd_enabled as u8) << 7;
        lcd_control |= (self.window_tile_map as u8) << 6;
        lcd_control |= (self.window_enabled as u8) << 5;
        lcd_control |= (self.background_and_window_location as u8) << 4;
        lcd_control |= (self.background_tile_map as u8) << 3;
        lcd_control |= (self.obj_size as u8) << 2;
        lcd_control |= (self.obj_display_enabled as u8) << 1;
        lcd_control |= self.background_enabled as u8;
        lcd_control
    }

    fn write_lcd_control(&mut self, val: u8) {
        self.lcd_enabled = (val >> 7) == 1;
        self.window_tile_map = TileMapLocation::from((val >> 6) & 1);
        self.window_enabled = ((val >> 5) & 1) == 1;
        self.background_and_window_location =
            BackgroundAndWindowLocation::from((val >> 4) & 1);
        self.background_tile_map = TileMapLocation::from((val >> 3) & 1);
        self.obj_size = ObjSize::from((val >> 2) & 1);
        self.obj_display_enabled = ((val >> 1) & 1) == 1;
        self.background_enabled = (val & 1) == 1;
    }

    fn read_lcd_stat(&self) -> u8 {
        let mut lcd_stat = 0x80; // bit 7 is always 1
        lcd_stat |= (self.coincidence_interrupt as u8) << 6;
        lcd_stat |= (self.oam_interrupt as u8) << 5;
        lcd_stat |= (self.vertical_blank_interrupt as u8) << 4;
        lcd_stat |= (self.horizontal_blank_interrupt as u8) << 3;
        lcd_stat |= ((self.scan_line == self.scan_line_compare) as u8) << 2;
        lcd_stat |= self.mode as u8;
        lcd_stat
    }

    /// Write the LCD stat register 0xFF41
    ///
    /// Bits 0-2 are read only
    fn write_lcd_stat(&mut self, val: u8) {
        self.coincidence_interrupt = (val >> 6) & 1 == 1;
        self.oam_interrupt = (val >> 5) & 1 == 1;
        self.vertical_blank_interrupt = (val >> 4) & 1 == 1;
        self.horizontal_blank_interrupt = (val >> 3) & 1 == 1;
    }
}

fn get_palette_color(color_num: u8, palette: u8) -> u8 {
    (palette >> (2 * color_num)) & 3
}