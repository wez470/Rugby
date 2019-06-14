pub const SAMPLE_BUFFER_SIZE: usize = 1024; // Number of samples in our audio buffer
const SAMPLE_RATE_CYCLES: usize = 95; // Number of cycles between samples to achieve at rate of 44100Hz
const WAVE_RAM_LENGTH: usize = 16; // Wave RAM can fit 32 4-bit samples

#[derive(Clone)]
pub struct Audio {
    pub channel3: Channel3,
    /// Vin to SO2 terminal enabled. Bit 7 at 0xFF24
    left_enabled: bool,
    /// S02 volume (0-7). Bits 4-6 at 0xFF24
    left_volume: u8,
    /// Vin to S01 terminal enabled. Bit 3 at 0xFF24
    right_enabled: bool,
    /// S02 volume (0-7). Bits 0-2 at 0xFF24
    right_volume: u8,
    /// Sound channel output selection. register 0xFF25
    selection: u8,
    /// Sound enabled. Bit 7 at 0xFF26. Cannot access any sound registers besides 0xFF26 while disabled.
    enabled: bool,
    /// Sound 4 enabled. Bit 3 at 0xFF26. Read only
    channel_4_enabled: bool,
    /// Sound 3 enabled. Bit 2 at 0xFF26. Read only
    channel_3_enabled: bool,
    /// Sound 2 enabled. Bit 1 at 0xFF26. Read only
    channel_2_enabled: bool,
    /// Sound 1 enabled. Bit 0 at 0xFF26. Read only
    channel_1_enabled: bool,
    // Audio output fields
    cycles: usize,
    pub buffer: Box<[u8]>,
    curr_buffer_pos: usize,
    nibble: usize,
    repeats: usize,
    last_played: u8,
}

impl Audio {
    pub fn new() -> Audio {
        Audio {
            channel3: Channel3::new(),
            cycles: 0,
            buffer: vec![0; SAMPLE_BUFFER_SIZE * 2].into_boxed_slice(),
            curr_buffer_pos: 0,
            nibble: 0,
            repeats: 0,
            last_played: 0,
            left_enabled: true,
            left_volume: 7,
            right_enabled: true,
            right_volume: 7,
            selection: 0xFF,
            enabled: true,
            channel_4_enabled: true,
            channel_3_enabled: true,
            channel_2_enabled: true,
            channel_1_enabled: true,
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x1A...0x1E => self.channel3.read_reg(addr),
            0x24 => {
                (self.left_enabled as u8) << 7
                | self.left_volume << 4
                | (self.right_enabled as u8) << 3
                | self.right_volume
            }
            0x25 => {
                self.selection
            }
            0x26 => {
                (self.enabled as u8) << 7
                | (self.channel_4_enabled as u8) << 3
                | (self.channel_3_enabled as u8) << 2
                | (self.channel_2_enabled as u8) << 1
                | (self.channel_1_enabled as u8)
            }
            0x30...0x3F => self.channel3.read_reg(addr),
            _ => panic!("Unimplemented audio register read"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x1A...0x1E => self.channel3.write_reg(addr, val),
            0x24 => {
                self.left_enabled = addr & (1 << 7) != 0;
                self.left_volume = (addr >> 4) & 0b111;
                self.left_enabled = addr & (1 << 3) != 0;
                self.left_volume = addr & 0b111;
            }
            0x25 => {
                self.selection = val;
            }
            0x26 => {
                // Should destroy all sound register contents upon disabled.
                self.enabled = val & (1 << 7) != 0;
            }
            0x30...0x3F => self.channel3.write_reg(addr, val),
            _ => panic!("Unimplemented audio register write"),
        }
    }

    pub fn step(&mut self, cycles: usize, audio_queue: &mut sdl2::audio::AudioQueue<u8>) {
        self.cycles += cycles;
        if self.cycles >= SAMPLE_RATE_CYCLES {
            self.cycles %= SAMPLE_RATE_CYCLES;
            self.repeats += 1;
            if self.repeats > 65536 / (2048 - self.channel3.frequency as usize) {
                if self.nibble == 0 {
                    self.last_played = (self.channel3.wave_ram[self.curr_buffer_pos] >> 4) / u8::from(self.channel3.volume);
                    self.nibble = 1
                }
                else {
                    self.last_played = (self.channel3.wave_ram[self.curr_buffer_pos] & 0b1111) / u8::from(self.channel3.volume);
                    self.nibble = 0;
                    self.curr_buffer_pos = (self.curr_buffer_pos + 1) % WAVE_RAM_LENGTH;
                }
                self.repeats = 0;
            }
            audio_queue.queue(&[((255 * (self.last_played as usize)) / 15) as u8]);
        }
    }
}

#[derive(Clone, Copy)]
pub enum Volume {
    Zero = 0,
    Full = 1,
    Half = 2,
    Quarter = 3,
}

impl std::convert::From<Volume> for f32 {
    fn from(value: Volume) -> f32 {
        match value {
            Volume::Zero => 0_f32,
            Volume::Full => 1_f32,
            Volume::Half => 0.5_f32,
            Volume::Quarter => 0.25_f32,
        }
    }
}

impl std::convert::From<Volume> for u8 {
    fn from(value: Volume) -> u8 {
        match value {
            Volume::Zero => 255,
            Volume::Full => 1,
            Volume::Half => 2,
            Volume::Quarter => 4,
        }
    }
}

impl std::convert::From<u8> for Volume {
    fn from(value: u8) -> Volume {
        match value {
            0 => Volume::Zero,
            1 => Volume::Full,
            2 => Volume::Half,
            3 => Volume::Quarter,
            _ => panic!("Invalid u8 value for volume")
        }
    }
}

#[derive(Clone)]
pub struct Channel3 {
    /// True if sound is on. Register FF1A
    pub on: bool,

    /// Sound Length. Register FF1B
    length: u8,

    /// Volume. Register FF1C
    pub volume: Volume,

    /// Frequency. Register FF1D and Bits 0-2 of Register FF1E
    pub frequency: u16,

    /// True if we are going to restart sound. TODO(wcarlson): What is this?
    restart: bool,

    /// True if we should stop after the current sound length
    stop: bool,

    /// Wave pattern RAM. Registers FF30-FF3F
    wave_ram: Box<[u8]>
}

impl Channel3 {
    pub fn new() -> Channel3 {
        Channel3 {
            on: false,
            length: 0,
            volume: Volume::Zero,
            frequency: 0,
            restart: false,
            stop: false,
            wave_ram: vec![0; WAVE_RAM_LENGTH].into_boxed_slice(),
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x1A => (self.on as u8) << 7,
            0x1B => self.length,
            0x1C => (self.volume as u8) << 5,
            0x1D => self.frequency as u8,
            0x1E => {
                let mut frequency_higher_data = 0b00111000; // Bits 3-5 unused
                frequency_higher_data |= (self.restart as u8) << 7;
                frequency_higher_data |= (self.stop as u8) << 6;
                frequency_higher_data |= ((self.frequency >> 8) as u8) & 0b111;
                frequency_higher_data
            },
            0x30...0x3F => self.wave_ram[(addr - 0x30) as usize],
            _ => panic!("Invalid read address for audio channel 3"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x1A => self.on = (val >> 7) == 1,
            0x1B => self.length = val,
            0x1C => self.volume = Volume::from((val >> 5) & 0b11),
            0x1D => {
                self.frequency &= !0 << 8;
                self.frequency |= val as u16
            },
            0x1E => {
                self.restart = (val >> 7) & 1 == 1;
                self.stop = (val >> 6) & 1 == 1;
                self.frequency &= 0xFF;
                self.frequency |= ((val & 0b111) as u16) << 8;
            },
            0x30...0x3F => self.wave_ram[(addr - 0x30) as usize] = val,
            _ => panic!("Invalid write address for audio channel 3"),
        }
    }
}