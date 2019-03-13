pub const SAMPLE_BUFFER_SIZE: usize = 1024; // Number of samples in our audio buffer
const SAMPLE_RATE_CYCLES: usize = 95; // Number of cycles between samples to achieve at rate of 44100Hz
const WAVE_RAM_LENGTH: usize = 16; // Wave RAM can fit 32 4-bit samples

#[derive(Clone)]
pub struct Audio {
    pub channel3: Channel3,
    cycles: usize,
    pub buffer: Box<[u8]>,
    curr_buffer_pos: usize,
}

impl Audio {
    pub fn new() -> Audio {
        Audio {
            channel3: Channel3::new(),
            cycles: 0,
            buffer: vec![0; SAMPLE_BUFFER_SIZE * 2].into_boxed_slice(),
            curr_buffer_pos: 0,
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x1A...0x1E => self.channel3.read_reg(addr),
            0x30...0x3F => self.channel3.read_reg(addr),
            _ => panic!("Unimplemented audio register read"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x1A...0x1E => self.channel3.write_reg(addr, val),
            0x30...0x3F => self.channel3.write_reg(addr, val),
            _ => panic!("Unimplemented audio register write"),
        }
    }

    pub fn step(&mut self, cycles: usize) {
        self.cycles += cycles;
        if self.cycles >= SAMPLE_RATE_CYCLES {
            self.cycles %= SAMPLE_RATE_CYCLES;
            for i in 0..WAVE_RAM_LENGTH {
                self.buffer[self.curr_buffer_pos] = self.channel3.wave_ram[i];
                self.curr_buffer_pos = (self.curr_buffer_pos + 1) % (SAMPLE_BUFFER_SIZE * 2);
            }
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
    wave_ram: Box<[u8]>,
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