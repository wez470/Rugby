use super::{LENGTH_COUNTER_RATE_CYCLES, EnvelopeDirection};

/// Max length for sound data
const MAX_SOUND_LENGTH: u8 = 64;

#[derive(Clone)]
pub struct Channel2 {
    /// Wave pattern. Bits 6-7 of 0xFF16
    wave_pattern: u8,

    /// Length of sound data. Bits 0-5 of 0xFF16
    length: u8,

    /// Length counter. Used to tell when to stop playing audio. Sound length is given by 64 - length.
    length_counter: u8,

    /// True if the length counter is enabled
    length_counter_enabled: bool,

    /// Volume. Bits 4-7 of 0xFF17
    volume: u8,

    /// Envelope direction. Bit 3 of 0xFF17.
    envelope_direction: EnvelopeDirection,

    /// Number of envelope sweeps. Bits 0-2 of 0xFF17
    envelope_sweeps: u8,

    /// Channel frequency. Lower bits are bits 0-7 of 0xFF18. Higher bits are 0-2 of 0xFF19
    /// Actual frequency is given by `(2048 - frequency) * 4`. http://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware
    frequency: u16,

    /// True if we are going to restart sound. Bit 7 of 0xFF19
    restart: bool,

    /// True if we should stop after the current sound length. Bit 6 of 0xFF19
    stop_after_sound_length: bool,

    /// Track current cycles for audio output
    curr_cycles: usize,

    /// Track current cycles for length counter
    curr_length_counter_cycles: usize,

    /// Track the wave pattern position
    curr_index: u8,

    /// Track the current audio output value
    curr_output: u8,

    /// True if the channel is enabled
    enabled: bool,
}

impl Channel2 {
    pub fn new() -> Channel2 {
        Channel2 {
            wave_pattern: 0,
            length: 0,
            length_counter: MAX_SOUND_LENGTH,
            length_counter_enabled: true,
            volume: 0,
            envelope_direction: EnvelopeDirection::Decrease,
            envelope_sweeps: 0,
            frequency: 0,
            restart: false,
            stop_after_sound_length: false,
            curr_cycles: 0,
            curr_length_counter_cycles: 0,
            curr_index: 0,
            curr_output: 0,
            enabled: true,
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x16 => (self.wave_pattern << 6) & self.length,
            0x17 => {
                self.volume << 4
                    | (self.envelope_direction as u8) << 3
                    | self.envelope_sweeps
            },
            0x18 => self.frequency as u8,
            0x19 => {
                0b00111000 // Bits 3-5 unused
                    | (self.restart as u8) << 7
                    | (self.stop_after_sound_length as u8) << 6
                    | ((self.frequency >> 8) as u8) & 0b111
            },
            _ => panic!("Invalid read address for audio channel 2"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x16 => {
                self.wave_pattern = val >> 6;
                self.length = val & 0b0011_1111;
                self.length_counter = MAX_SOUND_LENGTH - self.length;
            },
            0x17 => {
                self.envelope_sweeps = val & 0b0111;
                self.envelope_direction = EnvelopeDirection::from((val >> 3) & 1);
                self.volume = val >> 4;
            },
            0x18 => {
                self.frequency &= !0 << 8;
                self.frequency |= val as u16
            },
            0x19 => {
                let next_restart_val = (val >> 7) & 1 == 1;
                let next_stop_after_sound_length_val = (val >> 6) & 1 == 1;
                self.frequency &= 0xFF;
                self.frequency |= ((val & 0b111) as u16) << 8;
                self.enabled = next_restart_val;
                // TODO: Possibly disable sound here based on envelope

                if !self.stop_after_sound_length && next_stop_after_sound_length_val && self.length_counter % 2 == 0 && self.enabled {
                    // Extra length clock
                    self.update_length_counter(4);
                    // Sound disabled ony if trigger is clear
                    if next_restart_val {
                        self.enabled = true;
                    }
                }

                if next_restart_val && !self.enabled {
                    self.length_counter = MAX_SOUND_LENGTH;
                    if next_stop_after_sound_length_val && self.length_counter % 2 == 0 {
                        // Extra length clock
                        self.update_length_counter(4);
                    }
                }

                self.restart = next_restart_val;
                self.stop_after_sound_length = next_stop_after_sound_length_val;
            },
            _ => panic!("Invalid write address for audio channel 2"),
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

        self.curr_output * self.volume
    }

    fn get_wave_duty(&self) -> u8 {
        match self.wave_pattern {
            0 => 0b0000_0001,
            1 => 0b1000_0001,
            2 => 0b1000_0111,
            3 => 0b0111_1110,
            _ => panic!("Invalid channel 2 waveform value")
        }
    }

    fn update_length_counter(&mut self, cycles: usize) {
        self.curr_length_counter_cycles += cycles;
        if self.curr_length_counter_cycles >= LENGTH_COUNTER_RATE_CYCLES {
            self.curr_length_counter_cycles %= LENGTH_COUNTER_RATE_CYCLES;
            if self.length_counter > 0 && self.length_counter_enabled {
                self.length_counter -= 1;
                if self.length_counter == 0 {
                    self.enabled = false;
                }
            }
        }
    }
}
