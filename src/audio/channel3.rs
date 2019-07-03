/// Wave RAM can fit 32 4-bit samples
const WAVE_RAM_LENGTH: usize = 16;

/// Max length for sound data
const MAX_SOUND_LENGTH: u16 = 256;

#[derive(Clone)]
pub struct Channel3 {
    /// Sound Length. Register FF1B
    length: u8,

    /// Length counter. Used to tell when to stop playing audio. Sound length is given by 256 - length.
    length_counter: u16,

    /// True if the length counter is enabled
    length_counter_enabled: bool,

    /// Volume. Register FF1C
    pub volume: Volume,

    /// Frequency. Register FF1D and Bits 0-2 of Register FF1E
    /// Actual frequency is given by `(2048 - frequency) * 2`. http://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware
    pub frequency: u16,

    /// True if we are going to restart sound.
    restart: bool,

    /// True if we should stop after the current sound length
    stop: bool,

    /// Wave pattern RAM. Registers FF30-FF3F
    pub wave_ram: Box<[u8]>,

    /// Track current cycles for audio output
    cycles: usize,

    /// Track the current nibble index in wave ram
    curr_index: usize,

    /// Track the current audio output value
    curr_output: u8,

    /// True if the channel is enabled
    enabled: bool,
}

impl Channel3 {
    pub fn new() -> Channel3 {
        Channel3 {
            length: 0,
            length_counter: MAX_SOUND_LENGTH,
            length_counter_enabled: true,
            volume: Volume::Zero,
            frequency: 0,
            restart: false,
            stop: false,
            wave_ram: vec![0; WAVE_RAM_LENGTH].into_boxed_slice(),
            cycles: 0,
            curr_index: 0,
            curr_output: 0,
            enabled: true,
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x1A => (self.enabled as u8) << 7,
            0x1B => self.length,
            0x1C => (self.volume as u8) << 5,
            0x1D => self.frequency as u8,
            0x1E => {
                0b00111000 // Bits 3-5 unused
                | (self.restart as u8) << 7
                | (self.stop as u8) << 6
                | ((self.frequency >> 8) as u8) & 0b111
            },
            0x30...0x3F => self.wave_ram[(addr - 0x30) as usize],
            _ => panic!("Invalid read address for audio channel 3"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x1A => self.enabled = (val >> 7) == 1,
            0x1B => {
                self.length = val;
                self.length_counter = MAX_SOUND_LENGTH - self.length as u16;
            },
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

    pub fn step(&mut self, cycles: usize) -> u8 {
        if !self.enabled {
            return 0;
        }

        self.cycles += cycles;
        let freq = (2048 - self.frequency as usize) * 2;
        if self.cycles > freq && freq > 0 {
            self.cycles %= freq;
            let mut b = self.wave_ram[self.curr_index / 2];
            if self.curr_index % 2 == 0 {
                b = (b >> 4) & 0b1111;
            }
            else {
                b &= 0b1111;
            }
            self.curr_index = (self.curr_index + 1) % 32;
            match self.volume {
                Volume::Zero => self.curr_output = 0,
                Volume::Full => self.curr_output = b,
                Volume::Half => self.curr_output = b >> 1,
                Volume::Quarter => self.curr_output = b >> 2,
            }
        }
        self.curr_output
    }
}

#[derive(Clone, Copy)]
pub enum Volume {
    Zero = 0,
    Full = 1,
    Half = 2,
    Quarter = 3,
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
