use super::{EnvelopeDirection, LENGTH_COUNTER_RATE_CYCLES};

/// Max length for sound data
const MAX_SOUND_LENGTH: u8 = 64;

#[derive(Clone)]
pub struct Channel4 {
    /// Sound Length. Bits 0-5 of 0xFF20
    length: u8,

    /// Length counter. Used to tell when to stop playing audio. Sound length is given by 64 - length.
    length_counter: u8,

    /// True if the length counter is enabled
    length_counter_enabled: bool,

    /// Initial volume of envelope. Bits 4-7 of 0xFF21
    volume: u8,

    /// Envelope direction. Bit 3 of 0xFF21
    envelope_direction: EnvelopeDirection,

    /// Number of envelope sweeps. Bits 0-2 of 0xFF21
    envelope_sweeps: u8,

    /// Shift clock frequency. Bits 4-7 of 0xFF22
    shift_clock_frequency: u8,

    /// Counter step/width. Bit 3 of 0xFF22. 0=15 bits, 1=7 bits
    counter_step: u8,

    /// Dividing ratio of frequencies. Bits 0-2 of 0xFF22
    dividing_ratio: u8,

    /// 15 bit linear feedback shift register
    linear_feedback_shift_register: u16,

    /// True if we are going to restart sound.
    restart: bool,

    /// True if we should stop after the current sound length
    stop_after_sound_length: bool,

    /// Track the current step updating audio output
    curr_index: usize,

    /// Track current cycles for audio output
    curr_cycles: usize,

    /// Track the current audio output value
    curr_output: u8,

    /// True if the channel is enabled
    enabled: bool,
}

impl Channel4 {
    pub fn new() -> Self {
        Self {
            length: 0,
            length_counter: MAX_SOUND_LENGTH,
            length_counter_enabled: true,
            volume: 0,
            envelope_direction: EnvelopeDirection::Decrease,
            envelope_sweeps: 0,
            shift_clock_frequency: 0,
            counter_step: 0,
            linear_feedback_shift_register: 0b0111_1111_1111_1111,
            dividing_ratio: 0,
            restart: false,
            stop_after_sound_length: false,
            curr_index: 0,
            curr_cycles: 0,
            curr_output: 0,
            enabled: true,
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x20 => {
                self.length
            },
            0x21 => {
                self.volume << 4
                    | (self.envelope_direction as u8) << 3
                    | self.envelope_sweeps
            },
            0x22 => {
                self.shift_clock_frequency << 4
                    | self.counter_step << 3
                    | self.dividing_ratio
            },
            0x23 => {
                0b00111111 // Bits 0-5 unused
                    | (self.restart as u8) << 7
                    | (self.stop_after_sound_length as u8) << 6
            },
            _ => panic!("Invalid read address for audio channel 4"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x20 => {
                self.length = val & 0b0011_1111;
                self.length_counter = MAX_SOUND_LENGTH - self.length;
            },
            0x21 => {
                self.envelope_sweeps = val & 0b0111;
                self.envelope_direction = EnvelopeDirection::from((val >> 3) & 1);
                self.volume = val >> 4;
            },
            0x22 => {
                self.dividing_ratio = val & 0b0111;
                self.counter_step = (val >> 3) & 1;
                self.shift_clock_frequency = val >> 4;
            },
            0x23 => {
                self.restart = (val >> 7) & 1 == 1;
                self.stop_after_sound_length = (val >> 6) & 1 == 1;

                if self.length_counter == 0 && self.restart {
                    self.length_counter = 64;
                }
                if self.length_counter_enabled {
                    self.length_counter = 64
                }
                if self.restart {
                    self.linear_feedback_shift_register = 0b0111_1111_1111_1111;
                    self.enabled = true;
                    if self.stop_after_sound_length {
                        self.length_counter = 64
                    }
                }
                self.length_counter_enabled = self.restart;
            },
            _ => panic!("Invalid write address for audio channel 4"),
        }
    }

    pub fn step(&mut self, cycles: usize) -> u8 {
        if !self.enabled {
            return 0;
        }

        self.curr_index += 1;
        let out_freq = self.get_divisor(self.dividing_ratio) << self.shift_clock_frequency as usize;
        if self.curr_index >= out_freq {
            self.curr_index %= out_freq;
            self.curr_output = self.get_next_output();
        }

        self.curr_cycles += cycles;
        if self.curr_cycles >= LENGTH_COUNTER_RATE_CYCLES {
            self.curr_cycles %= LENGTH_COUNTER_RATE_CYCLES;
            if self.length_counter > 0 && self.length_counter_enabled {
                self.length_counter -= 1;
                if self.length_counter == 0 {
                    self.enabled = false;
                }
            }
        }

        self.curr_output * self.volume
    }

    fn get_divisor(&self, dividing_ratio: u8) -> usize {
        match dividing_ratio {
            0 => 8,
            1 => 16,
            2 => 32,
            3 => 48,
            4 => 64,
            5 => 80,
            6 => 96,
            7 => 112,
            _ => panic!("Invalid dividing ratio"),
        }
    }

    fn get_next_output(&mut self) -> u8 {
        let bit_0 = self.linear_feedback_shift_register & 1;
        let bit_1 = (self.linear_feedback_shift_register >> 1) & 1;
        let new_bit = bit_0 ^ bit_1;
        self.linear_feedback_shift_register = self.linear_feedback_shift_register >> 1;
        self.linear_feedback_shift_register &= 0b0011_1111_1111_1111;
        self.linear_feedback_shift_register |= new_bit << 14;
        if self.counter_step == 1 {
            self.linear_feedback_shift_register &= 0b0111_1111_1011_1111;
            self.linear_feedback_shift_register |= new_bit << 6;
        }
        return (!self.linear_feedback_shift_register & 1) as u8
    }
}
