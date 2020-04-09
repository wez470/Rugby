use super::{EnvelopeDirection, LENGTH_COUNTER_RATE_CYCLES};

/// Max length for sound data
const MAX_SOUND_LENGTH: u8 = 64;
const CPU_CLOCK_RATE: usize = 4194304;
const MAX_VOLUME: u8 = 15;

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
    curr_cycles: usize,

    /// Track current cycles for audio output
    curr_length_counter_cycles: usize,

    /// Track the current audio output value
    curr_output: u8,

    /// True if the channel is enabled
    enabled: bool,

    /// Volume countdown. WIP copied from Sameboy
    volume_countdown: u8,
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
            curr_cycles: 0,
            curr_length_counter_cycles: 0,
            curr_output: 0,
            enabled: false,
            volume_countdown: 0,
        }
    }

    pub fn enabled(&self) -> bool { self.enabled }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x20 => {
                println!("Read {:X}. val: {}", addr, 0xFF);
                0xFF
            }, // This entire register is write-only
            0x21 => {
                let val = self.volume << 4
                    | (self.envelope_direction as u8) << 3
                    | self.envelope_sweeps;
                println!("Read {:X}. val: {}", addr, val);
                val
            },
            0x22 => {
                let val = self.shift_clock_frequency << 4
                    | self.counter_step << 3
                    | self.dividing_ratio;
                println!("Read {:X}. val: {}", addr, val);
                val
            },
            0x23 => {
                let val = 0b10111111 // These bits are unused or write-only
                    | (self.restart as u8) << 7
                    | (self.stop_after_sound_length as u8) << 6;
                println!("Read {:X}. val: {}", addr, val);
                val
            },
            _ => panic!("Invalid read address for audio channel 4"),
        }
    }

    pub fn write_reg(&mut self, addr: u8, val: u8) {
        match addr {
            0x20 => {
                // self.length = val & 0b0011_1111;
                self.length = 63 & 0b0011_1111;
                self.length_counter = MAX_SOUND_LENGTH - self.length;
                // println!("Write NR41. Value: {}", val);
            },
            0x21 => {
                if self.envelope_sweeps != 0 && (val & 0b0111) == 0 {
                    // envelope disabled
                    self.volume_countdown = 0;
                }
                if (val & 0b1111_1000) == 0 {
                    // disables DAC
                    self.enabled = false;
                } else if self.enabled {
                    self.envelope_sweeps = val & 0b0111;
                    self.envelope_direction = EnvelopeDirection::from((val >> 3) & 1);
                    self.volume = val >> 4;
                }
                // println!("Write NR42. Value: {}", val);
            },
            0x22 => {
                self.dividing_ratio = val & 0b0111;
                self.counter_step = (val >> 3) & 1;
                self.shift_clock_frequency = val >> 4;
                // println!("Write NR43. Value: {}", val);
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
                // println!("Write NR44. Value: {}", val);

                // SAMEBOY NR44 WRITE CODE
                //
                // if (value & 0x80) {
                //     gb->apu.noise_channel.sample_countdown = (gb->apu.noise_channel.sample_length) * 2 + 6 - gb->apu.lf_div;
                //
                //     /* I'm COMPLETELY unsure about this logic, but it passes all relevant tests.
                //        See comment in NR43. */
                //     if ((gb->io_registers[GB_IO_NR43] & 7) && (gb->apu.noise_channel.alignment & 2) == 0) {
                //         if ((gb->io_registers[GB_IO_NR43] & 7) == 1) {
                //             gb->apu.noise_channel.sample_countdown += 2;
                //         }
                //         else {
                //             gb->apu.noise_channel.sample_countdown -= 2;
                //         }
                //     }
                //     if (gb->apu.is_active[GB_NOISE]) {
                //         gb->apu.noise_channel.sample_countdown += 2;
                //     }
                //
                //     gb->apu.noise_channel.current_volume = gb->io_registers[GB_IO_NR42] >> 4;
                //
                //     /* The volume changes caused by NRX4 sound start take effect instantly (i.e. the effect the previously
                //      started sound). The playback itself is not instant which is why we don't update the sample for other
                //      cases. */
                //     if (gb->apu.is_active[GB_NOISE]) {
                //         update_sample(gb, GB_NOISE,
                //                       gb->apu.current_lfsr_sample ?
                //         gb->apu.noise_channel.current_volume : 0,
                //         0);
                //     }
                //     gb->apu.noise_channel.lfsr = 0;
                //     gb->apu.current_lfsr_sample = false;
                //     gb->apu.noise_channel.volume_countdown = gb->io_registers[GB_IO_NR42] & 7;
                //
                //     if (!gb->apu.is_active[GB_NOISE] && (gb->io_registers[GB_IO_NR42] & 0xF8) != 0) {
                //         gb->apu.is_active[GB_NOISE] = true;
                //         update_sample(gb, GB_NOISE, 0, 0);
                //     }
                //
                //     if (gb->apu.noise_channel.pulse_length == 0) {
                //         gb->apu.noise_channel.pulse_length = 0x40;
                //         gb->apu.noise_channel.length_enabled = false;
                //     }
                // }
                //
                // /* APU glitch - if length is enabled while the DIV-divider's LSB is 1, tick the length once. */
                // if ((value & 0x40) &&
                //     !gb->apu.noise_channel.length_enabled &&
                //     (gb->apu.div_divider & 1) &&
                //     gb->apu.noise_channel.pulse_length) {
                //     gb->apu.noise_channel.pulse_length--;
                //     if (gb->apu.noise_channel.pulse_length == 0) {
                //         if (value & 0x80) {
                //             gb->apu.noise_channel.pulse_length = 0x3F;
                //         }
                //         else {
                //             gb->apu.is_active[GB_NOISE] = false;
                //             update_sample(gb, GB_NOISE, 0, 0);
                //         }
                //     }
                // }
                // gb->apu.noise_channel.length_enabled = value & 0x40;
            },
            _ => panic!("Invalid write address for audio channel 4"),
        }
    }

    pub fn step(&mut self, cycles: usize) -> f32 {
        if !self.enabled { return 0.0/0.0; }

        self.curr_cycles += cycles;
        // Frequency formula describe here: http://bgb.bircd.org/pandocs.htm#soundcontroller
        let out_freq = if self.dividing_ratio > 0 {
            524288 / (self.dividing_ratio as usize * (1 << (1 + self.shift_clock_frequency) as usize))
        } else {
            524288 / (0.5 * (1 << (1 + self.shift_clock_frequency) as usize) as f64) as usize
        };
        if self.curr_cycles >= (CPU_CLOCK_RATE / out_freq)  {
            self.curr_cycles %= CPU_CLOCK_RATE / out_freq;
            self.curr_output = self.get_next_output();
        }

        self.update_length_counter(cycles);

        if self.curr_output == 1 {
            (self.volume as f32) / MAX_VOLUME as f32
        } else {
            -(self.volume as f32) / MAX_VOLUME as f32
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
