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
    queue_cycles: usize,
}

impl Audio {
    pub fn new() -> Audio {
        Audio {
            channel3: Channel3::new(),
            queue_cycles: 0,
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
        if !self.enabled {
            return;
        }

        let channel3_val = self.channel3.step(cycles);

        let (mut left, mut right) = self.get_left_and_right_audio(channel3_val);
        left *= self.left_volume;
        right *= self.right_volume;

        self.output_to_queue(left, right, audio_queue, cycles);
    }

    fn get_left_and_right_audio(&self, channel_3_val: u8) -> (u8, u8) {
        let mut left = 0;
        let mut right = 0;

        if self.selection & (1 << 6) != 0 {
            left += channel_3_val;
        }
        if self.selection & (1 << 2) != 0 {
            right += channel_3_val;
        }
        (left, right)
    }

    fn output_to_queue(&mut self, left: u8, right: u8, queue: &mut sdl2::audio::AudioQueue<u8>, cycles: usize) {
        self.queue_cycles += cycles;
        if self.queue_cycles >= SAMPLE_RATE_CYCLES {
            self.queue_cycles %= SAMPLE_RATE_CYCLES;
            // Need to verify that this is the right way to do left and right audio
            queue.queue(&[left, right]);
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
    wave_ram: Box<[u8]>,

    /// Track current cycles for audio output
    cycles: usize,

    /// Track the current nibble index in wave ram
    curr_index: usize,

    /// Track the current audio output value
    curr_output: u8,
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
            cycles: 0,
            curr_index: 0,
            curr_output: 0,
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

    fn step(&mut self, cycles: usize) -> u8 {
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