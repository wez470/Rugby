use super::EnvelopeDirection;

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
    stop: bool,

    /// Track current cycles for audio output
    cycles: usize,

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
            length_counter: 64,
            length_counter_enabled: true,
            volume: 0,
            envelope_direction: EnvelopeDirection::Decrease,
            envelope_sweeps: 0,
            frequency: 0,
            restart: false,
            stop: false,
            cycles: 0,
            curr_index: 0,
            curr_output: 0,
            enabled: true,
        }
    }

    pub fn read_reg(&self, addr: u8) -> u8 {
        match addr {
            0x16 => self.wave_pattern << 6,
            0x17 => {
                self.volume << 4
                    | (self.envelope_direction as u8) << 3
                    | self.envelope_sweeps
            },
            0x18 => self.frequency as u8,
            0x19 => {
                0b00111000 // Bits 3-5 unused
                    | (self.restart as u8) << 7
                    | (self.stop as u8) << 6
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
                self.restart = (val >> 7) & 1 == 1;
                self.stop = (val >> 6) & 1 == 1;
                self.frequency &= 0xFF;
                self.frequency |= ((val & 0b111) as u16) << 8;
            },
            _ => panic!("Invalid write address for audio channel 2"),
        }
    }

    pub fn step(&mut self, cycles: usize) -> u8 {
        self.cycles += cycles;
        let freq = (2048 - self.frequency as usize) * 4;
        if self.cycles > freq && freq > 0 {
            self.cycles %= freq;
            self.curr_output = (self.get_wave_duty() >> self.curr_index) & 1;
            self.curr_index = (self.curr_index + 1) % 8;
        }
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
}
