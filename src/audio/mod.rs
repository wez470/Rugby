mod channel1;
mod channel2;
mod channel3;
mod channel4;

use channel1::Channel1;
use channel2::Channel2;
use channel3::Channel3;
use channel4::Channel4;
use log::warn;

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
            enabled: false,
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
                | (self.channel4.enabled() as u8) << 3
                | (self.channel3.enabled() as u8) << 2
                | (self.channel2.enabled() as u8) << 1
                | (self.channel1.enabled() as u8)
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
        fn convert(sample: u8) -> f32 {
            assert!(sample <= 15);
            (sample as f32) * (2.0 / 15.0) - 1.0
        }

        assert!(convert(0) == -1.0);
        assert!(convert(15) == 1.0);

        let channel1_val = self.channel1.step(cycles);
        let channel2_val = self.channel2.step(cycles);
        // let channel3_val = convert(self.channel3.step(cycles));
        let channel3_val = 0.0;
        let channel4_val = self.channel4.step(cycles);

        let (mut left, mut right) = self.get_left_and_right_audio(channel1_val, channel2_val, channel3_val, channel4_val);

        // TODO(solson): Re-investigate how master volume is supposed to work.
        left *= (self.left_volume as f32 + 1.0) / 8.0;
        right *= (self.right_volume as f32 + 1.0) / 8.0;


        self.output_to_queue(left, right, audio_queue, cycles);
    }

    fn get_left_and_right_audio(
        &self,
        channel1_val: f32,
        channel2_val: f32,
        channel3_val: f32,
        channel4_val: f32,
    ) -> (f32, f32) {
        if !self.enabled { return (0.0, 0.0); }

        let mut left: f32 = 0.0;
        let mut right: f32 = 0.0;

        if self.channel1.enabled() && !self.channel_1_muted {
            if self.selection & (1 << 4) != 0 {
                left += channel1_val;
                log::trace!("channel 1: {}", channel1_val);
            }
            if self.selection & 1 != 0 {
                right += channel1_val;
            }
        }
        if self.channel2.enabled() && !self.channel_2_muted {
            if self.selection & (1 << 5) != 0 {
                left += channel2_val;
                log::trace!("channel 2: {}", channel2_val);
            }
            if self.selection & (1 << 1) != 0 {
                right += channel2_val;
            }
        }
        if self.channel3.enabled() && !self.channel_3_muted {
            if self.selection & (1 << 6) != 0 {
                left += channel3_val;
                log::trace!("channel 3: {}", channel3_val);
            }
            if self.selection & (1 << 2) != 0 {
                right += channel3_val;
            }
        }
        if self.channel4.enabled() && !self.channel_4_muted {
            if self.selection & (1 << 7) != 0 {
                left += channel4_val;
                log::trace!("channel 4: {}", channel4_val);
            }
            if self.selection & (1 << 3) != 0 {
                right += channel4_val;
            }
        }

        log::trace!("overall:   {}", left / 4.0);

        (left / 4.0, right / 4.0)
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

#[derive(Clone, Copy)]
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

fn gb_sample_to_float(s: u8) -> f32 {
    -1.0 + (s as f32) * (2.0/15.0)
}

// /// A 4-bit integer Game Boy audio sample, ranging from 0 to 15.
// ///
// /// Our current assumption about how the values map to standard floating-point
// /// samples is as follows:
// ///   * 0 maps to -1.0
// ///   * 15 maps to 1.0
// ///   * The other values are perfectly linearly spaced
// ///
// /// In particular, this means that the step size between adjacent samples is
// /// `2/15` or `0.1(3)`. In general, sample `n` maps to `-1.0 + n*(2/15)`. For
// /// example, sample 1 is `-1.0 + 0.1(3) == -0.8(6)`.
// ///
// /// Note that this interpretation means there is no neutral sample mapping to
// /// 0.0, since sample 7 is `-0.0(6)` and sample 8 is `+0.0(6)`. This shouldn't be
// /// a huge problem, since when channels aren't playing, they are turned off
// /// completely, and we can treat that state as a true 0.0 level.
// ///
// /// This assumption is based on blargg's sound documentation at
// /// https://gist.github.com/drhelius/3652407 or
// /// http://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Channel_DAC.
// #[derive(Clone, Copy, Debug)]
// pub struct Sample(u8);

// impl Sample {
//     /// Create a new sample from the given byte, which *must* be between 0 and 15
//     /// inclusive, or else we panic.
//     fn new(s: u8) -> Sample {
//         assert!(s <= 15);
//         Sample(s)
//     }
// }

// impl std::convert::From<Sample> for f32 {
//     fn from(s: Sample) -> f32 {
//         debug_assert!(s.0 <= 15);
//         -1.0 + (s.0 as f32) * (2.0/15.0)
//     }
// }

// #[test]
// fn sample_test() {
//     // Test all the values mentioned in the `Sample` doc comment.
//     use approx::assert_relative_eq;
//     assert_relative_eq!(f32::from(Sample::new(0)), -1.0);
//     assert_relative_eq!(f32::from(Sample::new(1)), -0.86666666666666666666);
//     assert_relative_eq!(f32::from(Sample::new(7)), -0.06666666666666666666);
//     assert_relative_eq!(f32::from(Sample::new(8)), 0.06666666666666666666);
//     assert_relative_eq!(f32::from(Sample::new(15)), 1.0);
// }
