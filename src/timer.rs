use crate::interrupts::Interrupt;

const CYCLES_PER_DIVIDE_INC: usize = 256; // The number of cycles between increments of the divide register

#[derive(Clone, Copy)]
enum CounterSpeed {
    S4096 = 0,
    S262144 = 1,
    S65536 = 2,
    S16384 = 3,
}

impl std::convert::From<CounterSpeed> for usize {
    fn from(value: CounterSpeed) -> usize {
        match value {
            CounterSpeed::S4096 => 1024,
            CounterSpeed::S262144 => 16,
            CounterSpeed::S65536 => 64,
            CounterSpeed::S16384 => 256,
        }
    }
}

impl std::convert::From<u8> for CounterSpeed {
    fn from(value: u8) -> CounterSpeed {
        match value {
            0 => CounterSpeed::S4096,
            1 => CounterSpeed::S262144,
            2 => CounterSpeed::S65536,
            3 => CounterSpeed::S16384,
            _ => panic!("Invalid number for CounterSpeed"),
        }
    }
}

#[derive(Clone)]
pub struct Timer {
    /// The divider `DIV` register 0xFF04
    divider: u8,

    /// The divider register cycle counter
    div_cycle_counter: usize,

    /// The timer counter `TIMA` register 0xFF05
    counter: u8,

    ///  The timer counter register cycle counter
    counter_cycle_counter: usize,

    /// The timer modulo `TMA` register 0xFF06
    modulo: u8,

    /// The timer control `TAC` register 0xFF07 bit 2
    counter_running: bool,

    /// The timer control `TAC` register 0xFF07 bits 0-1
    counter_speed: CounterSpeed,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            divider: 0,
            div_cycle_counter: 0,
            counter: 0,
            counter_cycle_counter: 0,
            modulo: 0,
            counter_running: false,
            counter_speed: CounterSpeed::S4096,
        }
    }

    pub fn step(&mut self, cycles: usize) -> Option<Interrupt> {
        {
            self.update_divider(cycles);
        }
        return self.update_counter(cycles);
    }

    fn update_divider(&mut self, cycles: usize) {
        self.div_cycle_counter += cycles;
        if self.div_cycle_counter >= CYCLES_PER_DIVIDE_INC {
            self.div_cycle_counter %= CYCLES_PER_DIVIDE_INC;
            self.divider = self.divider.wrapping_add(1);
        }
    }

    fn update_counter(&mut self, cycles: usize) -> Option<Interrupt> {
        if !self.counter_running {
            return None;
        }
        self.counter_cycle_counter += cycles;
        if self.counter_cycle_counter >= usize::from(self.counter_speed) {
            self.counter_cycle_counter %= usize::from(self.counter_speed);
            let (new_counter, overflow) = self.counter.overflowing_add(1);
            if overflow {
                self.counter = self.modulo;
                return Some(Interrupt::Timer);
            } else {
                self.counter = new_counter;
            }
        }
        None
    }

    pub fn read_mem(&self, addr: u8) -> u8 {
        match addr {
            0x04 => self.divider,
            0x05 => self.counter,
            0x06 => self.modulo,
            // The upper 5 bits are unused and always 1.
            0x07 => 0b1111_1000 | (self.counter_running as u8) << 2 | self.counter_speed as u8,
            _ => panic!("Invalid read address for timer")
        }
    }

    pub fn write_mem(&mut self, addr: u8, val: u8) {
        match addr {
            0x04 => {
                self.div_cycle_counter = 0;
                self.divider = 0
            },
            0x05 => self.counter = val,
            0x06 => self.modulo = val,
            0x07 => {
                self.counter_speed = CounterSpeed::from(val & 3);
                self.counter_running = ((val >> 2) & 1) == 1;
            },
            _ => panic!("Invalid read address for timer")
        }
    }
}