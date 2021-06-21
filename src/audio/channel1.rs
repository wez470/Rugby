use log::warn;
use super::{CYCLES_PER_SECOND, LENGTH_COUNTER_RATE_CYCLES, EnvelopeDirection};

/// Max length for sound data
const MAX_SOUND_LENGTH: u8 = 64;

#[derive(Clone)]
pub struct Channel1 {
    // TODO(wcarlson): Sweep register

    /// Wave pattern duty. Bits 6-7 of 0xFF11
    wave_pattern: u8,

    /// Length counter. Used to tell when to stop playing audio.
    /// Sound length is given by 64 - bits 5-0 of 0xFF11.
    length_counter: u8,

    /// True if the length counter is enabled
    length_counter_enabled: bool,

    /// Volume. Bits 4-7 of 0xFF12
    initial_volume: u8,

    /// Current volume after being changed by the envelope
    curr_volume: u8,

    /// Envelope direction. Bit 3 of 0xFF12.
    envelope_direction: EnvelopeDirection,

    /// Number of envelope sweeps. Bits 0-2 of 0xFF12
    envelope_sweeps: u8,

    /// Channel frequency. Lower bits are bits 0-7 of 0xFF13. Higher bits are 0-2 of 0xFF14
    /// Actual frequency is given by `(2048 - frequency) * 4`. http://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware
    frequency: u16,

    /// True if we are going to restart sound. Bit 7 of 0xFF14
    restart: bool,

    /// True if we should stop after the current sound length. Bit 6 of 0xFF14
    stop_after_sound_length: bool,

    /// Track current cycles for audio output
    curr_cycles: usize,

    /// Track current cycles for volume envelope sweep
    curr_volume_cycles: usize,

    /// Track current cycles for length counter
    curr_length_counter_cycles: usize,

    /// Track the wave pattern position
    curr_index: u8,

    /// Track the current audio output value
    curr_output: u8,

    /// True if the channel is enabled
    enabled: bool,
}

impl Channel1 {
    pub fn new() -> Channel1 {
        Channel1 {
            wave_pattern: 0b10,
            length_counter: MAX_SOUND_LENGTH,
            length_counter_enabled: true,
            initial_volume: 0b11111,
            curr_volume: 0, // TODO: check what this should be for channels 1,2,4
            envelope_direction: EnvelopeDirection::Decrease,
            envelope_sweeps: 0b11,
            frequency: 0,
            restart: false,
            stop_after_sound_length: false,
            curr_cycles: 0,
            curr_volume_cycles: 0,
            curr_length_counter_cycles: 0,
            curr_index: 0,
            curr_output: 0,
            enabled: false,
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x10 => {
                warn!("unimplemented read from channel 1 audio sweep register");
                0x80
            }
            0x11 => (self.wave_pattern << 6) | 0b0011_1111, // Low bits are write-only
            0x12 => {
                self.initial_volume << 4
                    | (self.envelope_direction as u8) << 3
                    | self.envelope_sweeps
            },
            0x13 => 0xFF, // This register is entirely write-only
            0x14 => {
                0b10111111 // These bits are unused or write-only
                    | (self.stop_after_sound_length as u8) << 6
            }
            _ => panic!("Invalid read address for audio channel 1"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x10 => warn!("unimplemented write to channel 1 audio sweep register"),
            0x11 => {
                self.wave_pattern = val >> 6;
                self.length_counter = MAX_SOUND_LENGTH - (val & 0b0011_1111);
            },
            0x12 => {
                self.envelope_sweeps = val & 0b0111;
                self.envelope_direction = EnvelopeDirection::from((val >> 3) & 1);
                self.initial_volume = val >> 4;
            },
            0x13 => {
                self.frequency &= !0 << 8;
                self.frequency |= val as u16
            },
            0x14 => {
                self.restart = (val >> 7) & 1 == 1;
                self.stop_after_sound_length = (val >> 6) & 1 == 1;
                self.frequency &= 0xFF;
                self.frequency |= ((val & 0b111) as u16) << 8;

                if self.length_counter == 0 && self.restart {
                    self.length_counter = MAX_SOUND_LENGTH;
                }
                // if self.length_counter_enabled {
                //     self.length_counter = MAX_SOUND_LENGTH
                // }
                if self.restart {
                    self.enabled = true;
                    self.curr_volume = self.initial_volume;
                    if self.stop_after_sound_length {
                        self.length_counter = MAX_SOUND_LENGTH
                    }
                }
                self.length_counter_enabled = self.restart;
            },
            _ => panic!("Invalid write address for audio channel 1"),
        }
    }

    pub fn step(&mut self, cycles: usize) -> u8 {
        if !self.enabled {
            return 0;
        }

        self.curr_cycles += cycles;
        let freq = (2048 - self.frequency as usize) * 4;
        if self.curr_cycles > freq && freq > 0 {
            self.curr_cycles %= freq;
            self.curr_output = (self.get_wave_duty() >> self.curr_index) & 1;
            self.curr_index = (self.curr_index + 1) % 8;
        }

        self.update_length_counter(cycles);
        self.update_volume(cycles);

        self.curr_output * self.curr_volume
    }

    fn get_wave_duty(&self) -> u8 {
        match self.wave_pattern {
            0 => 0b0000_0001,
            1 => 0b1000_0001,
            2 => 0b1000_0111,
            3 => 0b0111_1110,
            _ => panic!("Invalid channel 1 waveform value")
        }
    }

    fn update_length_counter(&mut self, cycles: usize) {
        self.curr_length_counter_cycles += cycles;
        if self.curr_length_counter_cycles >= LENGTH_COUNTER_RATE_CYCLES {
            self.curr_length_counter_cycles %= LENGTH_COUNTER_RATE_CYCLES;
            if self.length_counter > 0 && self.stop_after_sound_length {
                self.length_counter -= 1;
                if self.length_counter == 0 {
                    self.enabled = false;
                }
            }
        }
    }

    fn update_volume(&mut self, cycles: usize) {
        if self.curr_volume == 0 && self.envelope_direction == EnvelopeDirection::Decrease ||
            self.curr_volume == 15 && self.envelope_direction == EnvelopeDirection::Increase ||
            self.envelope_sweeps == 0 {
            return
        }
        self.curr_volume_cycles += cycles;
        if self.curr_volume_cycles >= self.envelope_sweeps as usize * CYCLES_PER_SECOND / 64 {
            self.curr_volume_cycles %= self.envelope_sweeps as usize * CYCLES_PER_SECOND / 64;
            let vol_adjustment = if self.envelope_direction == EnvelopeDirection::Increase { 1 } else { -1 };
            self.curr_volume = (self.curr_volume as i32 + vol_adjustment) as u8;
        }
    }
}
