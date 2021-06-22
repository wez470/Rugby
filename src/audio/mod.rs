mod channel1;
mod channel2;
mod channel3;
mod channel4;

use channel1::Channel1;
use channel2::Channel2;
use channel3::Channel3;
use channel4::Channel4;
use log::warn;

/// CPU cycles per second for the Gamboy
/// TODO(wcarlson): move this somewhere else?
const CYCLES_PER_SECOND: usize = 4194304;

/// Number of samples in our audio buffer
pub const SAMPLE_BUFFER_SIZE: usize = 1024;

/// Number of cycles between samples to achieve at rate of 44100Hz
const SAMPLE_RATE_CYCLES: usize = 95;

/// Number of cycles between length counter updates to achieve 256Hz
const LENGTH_COUNTER_RATE_CYCLES: usize = 16383;

#[derive(Clone)]
pub struct Audio {
    pub channel1: Channel1,
    pub channel2: Channel2,
    pub channel3: Channel3,
    pub channel4: Channel4,
    /// Vin to SO2 terminal enabled. Bit 7 at 0xFF24
    output_vin_left: bool,
    /// S02 volume (0-7). Bits 4-6 at 0xFF24
    left_volume: u8,
    /// Vin to S01 terminal enabled. Bit 3 at 0xFF24
    output_vin_right: bool,
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
    /// Cycle counter for outputting sound data to the audio queue at the proper rate.
    queue_cycles: usize,

    /// Debug features
    /// Whether or not to mute in a specified channel
    pub channel_1_muted: bool,
    pub channel_2_muted: bool,
    pub channel_3_muted: bool,
    pub channel_4_muted: bool,
}

impl Audio {
    pub fn new() -> Audio {
        Audio {
            channel1: Channel1::new(),
            channel2: Channel2::new(),
            channel3: Channel3::new(),
            channel4: Channel4::new(),
            queue_cycles: 0,
            output_vin_left: false,
            left_volume: 7,
            output_vin_right: false,
            right_volume: 7,
            selection: 0xF3,
            enabled: true,
            channel_4_enabled: true,
            channel_3_enabled: true,
            channel_2_enabled: true,
            channel_1_enabled: true,
            channel_4_muted: false,
            channel_3_muted: false,
            channel_2_muted: false,
            channel_1_muted: false,
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x10..=0x14 => self.channel1.read_reg(addr),
            0x16..=0x19 => self.channel2.read_reg(addr),
            0x1A..=0x1E => self.channel3.read_reg(addr),
            0x20..=0x23 => self.channel4.read_reg(addr),
            0x24 => {
                (self.output_vin_left as u8) << 7
                | self.left_volume << 4
                | (self.output_vin_right as u8) << 3
                | self.right_volume
            }
            0x25 => {
                self.selection
            }
            0x26 => {
                (self.enabled as u8) << 7
                | 0b0111_0000 // Unused bits
                | (self.channel_4_enabled as u8) << 3
                | (self.channel_3_enabled as u8) << 2
                | (self.channel_2_enabled as u8) << 1
                | (self.channel_1_enabled as u8)
            }
            0x30..=0x3F => self.channel3.read_reg(addr),
            _ => panic!("Unimplemented audio register read"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x10..=0x14 => self.channel1.write_reg(addr, val),
            0x16..=0x19 => self.channel2.write_reg(addr, val),
            0x1A..=0x1E => self.channel3.write_reg(addr, val),
            0x20..=0x23 => self.channel4.write_reg(addr, val),
            0x24 => {
                self.output_vin_left = val & (1 << 7) != 0;
                self.left_volume = (val >> 4) & 0b111;
                self.output_vin_right = val & (1 << 3) != 0;
                self.right_volume = val & 0b111;
                if self.output_vin_left || self.output_vin_right {
                    warn!("Game wants us to output the Vin audio signal from \
                        the cartridge, which is completely unimplemented");
                }
            }
            0x25 => {
                self.selection = val;
            }
            0x26 => {
                // Should destroy all sound register contents upon disabled.
                self.enabled = val & (1 << 7) != 0;
            }
            0x30..=0x3F => self.channel3.write_reg(addr, val),
            _ => panic!("Unimplemented audio register write"),
        }
    }

    pub fn step(&mut self, cycles: usize, audio_queue: &mut sdl2::audio::AudioQueue<f32>) {
        let channel1_val = self.channel1.step(cycles);
        let channel2_val = self.channel2.step(cycles);
        let channel3_val = self.channel3.step(cycles);
        let channel4_val = self.channel4.step(cycles);

        let (left, right) = self.get_left_and_right_audio(channel1_val, channel2_val, channel3_val, channel4_val);
        // TODO: revisit master volume
        let left_float = gb_sample_to_float(left) * (self.left_volume as f32 + 1.0) / 8.0;
        let right_float = gb_sample_to_float(right) * (self.right_volume as f32 + 1.0) / 8.0;

        self.output_to_queue(left_float, right_float, audio_queue, cycles);
    }

    fn get_left_and_right_audio(&self, channel1_val: u8, channel2_val: u8, channel3_val: u8, channel4_val: u8) -> (u8, u8) {
        let mut left: u16 = 0;
        let mut right: u16 = 0;
        if !self.enabled {
            return (0, 0);
        }

        if self.channel_4_enabled && !self.channel_4_muted {
            if self.selection & (1 << 7) != 0 {
                left += channel4_val as u16;
            }
        }
        if self.channel_3_enabled && !self.channel_3_muted {
            if self.selection & (1 << 6) != 0 {
                left += channel3_val as u16;
            }
        }
        if self.channel_2_enabled && !self.channel_2_muted {
            if self.selection & (1 << 5) != 0 {
                left += channel2_val as u16;
            }
        }
        if self.channel_1_enabled && !self.channel_1_muted {
            if self.selection & (1 << 4) != 0 {
                left += channel1_val as u16;
            }
        }

        if self.channel_4_enabled && !self.channel_4_muted {
            if self.selection & (1 << 3) != 0 {
                right += channel4_val as u16;
            }
        }
        if self.channel_3_enabled && !self.channel_3_muted {
            if self.selection & (1 << 2) != 0 {
                right += channel3_val as u16;
            }
        }
        if self.channel_2_enabled && !self.channel_2_muted {
            if self.selection & (1 << 1) != 0 {
                right += channel2_val as u16;
            }
        }
        if self.channel_1_enabled && !self.channel_1_muted {
            if self.selection & 1 != 0 {
                right += channel1_val as u16;
            }
        }

        ((left / 4) as u8, (right / 4) as u8)
    }

    fn output_to_queue(&mut self, left: f32, right: f32, queue: &mut sdl2::audio::AudioQueue<f32>, cycles: usize) {
        self.queue_cycles += cycles;
        if self.queue_cycles >= SAMPLE_RATE_CYCLES {
            self.queue_cycles %= SAMPLE_RATE_CYCLES;
            // Need to verify that this is the right way to do left and right audio
            queue.queue(&[left, right]);
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum EnvelopeDirection {
    Decrease = 0,
    Increase = 1,
}

impl std::convert::From<u8> for EnvelopeDirection {
    fn from(value: u8) -> EnvelopeDirection {
        match value {
            0 => EnvelopeDirection::Decrease,
            1 => EnvelopeDirection::Increase,
            _ => panic!("Invalid u8 value for envelope direction")
        }
    }
}

// Converts the standard 0 to 15 (4-bit) GB sample to a range of -1.0 to 1.0
fn gb_sample_to_float(s: u8) -> f32 {
    -1.0 + (s as f32) * (2.0/15.0)
}