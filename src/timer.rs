use crate::interrupts::Interrupt;

const CYCLES_PER_DIVIDE_INC: usize = 16384; // The number of cycles between increments of the divide register

#[derive(Clone)]
pub struct Timer {
    /// The divider register 0xFF04
    divider: u8,

    /// The divider register cycle counter
    div_cycle_counter: usize,

    counter: u8,
    modulo: u8,
    control: u8,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            divider: 0,
            div_cycle_counter: 0,
            counter: 0,
            modulo: 0,
            control: 0
        }
    }

    pub fn step(&mut self, cycles: u8) -> Option<Interrupt> {
        {
            self.update_divider(cycles);
        }
        None
    }

    fn update_divider(&mut self, cycles: u8) {
        self.div_cycle_counter += cycles as usize;
        if self.div_cycle_counter >= CYCLES_PER_DIVIDE_INC {
            self.divider = self.divider.wrapping_add(1);
            self.div_cycle_counter %= CYCLES_PER_DIVIDE_INC;
        }
    }

    pub fn read_divider(& self) -> u8 {
        self.divider
    }

    pub fn write_divider(&mut self) {
        self.div_cycle_counter = 0;
        self.divider = 0;
    }
}