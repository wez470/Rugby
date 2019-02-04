use crate::cart_header::{CartHeader, MbcType};

const ROM_BANK_SIZE: usize = 0x4000;
const RAM_BANK_SIZE: usize = 0x2000;

#[derive(Clone, Debug)]
pub struct Cart {
    // TODO(solson): Can this be private? It's used by tests in cpu::tests.
    pub rom: Box<[u8]>,
    ram: Box<[u8]>,
    mbc: Mbc,
}

#[derive(Clone, Debug)]
pub enum CartError {
    ProvidedRamWrongSize {
        expected: usize,
        actual: usize,
    }
}

impl Cart {
    pub fn new(
        rom: Box<[u8]>,
        ram_opt: Option<Box<[u8]>>,
        cart_header: &CartHeader,
    ) -> Result<Cart, CartError> {
        if let Some(ref ram) = ram_opt {
            if ram.len() != cart_header.ram_size {
                return Err(CartError::ProvidedRamWrongSize {
                    expected: cart_header.ram_size,
                    actual: ram.len(),
                });
            }
        }
        Ok(Cart {
            rom,
            ram: ram_opt.unwrap_or_else(|| vec![0; cart_header.ram_size].into_boxed_slice()),
            mbc: Mbc::new(cart_header.cart_type.mbc)
        })
    }

    pub fn read(&self, addr: u16) -> u8 {
        match self.mbc {
            Mbc::None => read_mbc_none(&self.rom, &self.ram, addr),
            Mbc::Mbc1(ref mbc1) => mbc1.read(&self.rom, &self.ram, addr),
            Mbc::Mbc3(ref mbc3) => mbc3.read(&self.rom, &self.ram, addr),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match self.mbc {
            Mbc::None => write_mbc_none(&mut self.ram, addr, val),
            Mbc::Mbc1(ref mut mbc1) => mbc1.write(&mut self.ram, addr, val),
            Mbc::Mbc3(ref mut mbc3) => mbc3.write(&mut self.ram, addr, val),
        }
    }

    pub fn ram(&self) -> &[u8] { &self.ram }
}

#[derive(Clone, Debug)]
enum Mbc {
    None,
    Mbc1(Mbc1),
    Mbc3(Mbc3),
}

impl Mbc {
    fn new(mbc_type: MbcType) -> Mbc {
        match mbc_type {
            MbcType::NoMbc => Mbc::None,

            MbcType::Mbc1 => Mbc::Mbc1(Mbc1 {
                mode: Mbc1Mode::Rom,
                ram_enabled: false,
                bank_reg1: 1,
                bank_reg2: 0,
            }),

            MbcType::Mbc3 => Mbc::Mbc3(Mbc3 {
                rom_bank: 1,
                ram_rtc_enabled: false,
                ram_rtc_bank: 0,
                rtc: [0; 5],
            }),

            _ => panic!("Unimplemented Mbc Type!"),
        }
    }
}

fn read_mbc_none(rom: &[u8], ram: &[u8], addr: u16) -> u8 {
    match addr {
        // ROM
        0x0000...0x7FFF => rom[addr as usize],

        // RAM
        0xA000...0xBFFF => {
            let ram_index = (addr - 0xA000) as usize;
            // If the RAM is not present, the hardware returns all bits set.
            *ram.get(ram_index).unwrap_or(&0xFF)
        }

        _ => panic!("Unimplemented Mbc::None read address: {}", addr),
    }
}

fn write_mbc_none(ram: &mut [u8], addr: u16, val: u8) {
    match addr {
        // ROM: Writes are ignored.
        0x0000...0x7FFF => {}

        // RAM
        0xA000...0xBFFF => {
            let ram_index = (addr - 0xA000) as usize;
            // If the RAM is not present, the hardware ignores writes.
            if let Some(p) = ram.get_mut(ram_index) {
                *p = val;
            }
        }

        _ => panic!("Unimplemented Mbc::None read address: {}", addr),
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Mbc1Mode {
    Rom,
    Ram,
}

#[derive(Clone, Debug)]
struct Mbc1 {
    mode: Mbc1Mode,
    ram_enabled: bool,
    bank_reg1: u8,
    bank_reg2: u8,
}

impl Mbc1 {
    fn rom_index(&self, rom: &[u8], bank: u8, addr: u16) -> usize {
        let bank_base = bank as usize * ROM_BANK_SIZE;
        let addr_in_bank = addr as usize & (ROM_BANK_SIZE - 1);
        (bank_base | addr_in_bank) & (rom.len() - 1)
    }

    fn ram_index(&self, ram: &[u8], bank: u8, addr: u16) -> usize {
        let bank_base = bank as usize * RAM_BANK_SIZE;
        let addr_in_bank = addr as usize & (RAM_BANK_SIZE - 1);
        (bank_base | addr_in_bank) & (ram.len() - 1)
    }

    fn read(&self, rom: &[u8], ram: &[u8], addr: u16) -> u8 {
        match addr {
            0x0000...0x3FFF => {
                let bank = match self.mode {
                    Mbc1Mode::Rom => 0,
                    Mbc1Mode::Ram => self.bank_reg2 << 5,
                };
                rom[self.rom_index(rom, bank, addr)]
            }

            0x4000...0x7FFF => {
                let bank = self.bank_reg2 << 5 | self.bank_reg1;
                rom[self.rom_index(rom, bank, addr)]
            }

            0xA000...0xBFFF => {
                // When RAM is disabled, the hardware returns all bits set.
                if !self.ram_enabled || ram.len() == 0 { return 0xFF; }
                let bank = match self.mode {
                    Mbc1Mode::Rom => 0,
                    Mbc1Mode::Ram => self.bank_reg2,
                };
                ram[self.ram_index(ram, bank, addr)]
            }

            _ => panic!("Unimplemented MBC1 read at address: {}", addr),
        }
    }

    fn write(&mut self, ram: &mut [u8], addr: u16, val: u8) {
        match addr {
            // RAM Enable
            0x0000...0x1FFF => {
                self.ram_enabled = (val & 0b1111) == 0b1010;
            }

            // ROM bank lower bits write
            0x2000...0x3FFF => {
                self.bank_reg1 = val & 0b0001_1111;
                if self.bank_reg1 == 0 {
                    self.bank_reg1 = 1;
                }
            }

            // RAM Bank / Upper ROM bank bits write
            0x4000...0x5FFF => {
                self.bank_reg2 = val & 0b0011;
            }

            // ROM / RAM Mode
            0x6000...0x7FFF => {
                self.mode = if val & 1 == 0 { Mbc1Mode::Rom } else { Mbc1Mode::Ram };
            }

            // Switchable RAM bank
            0xA000...0xBFFF => {
                // When RAM is disabled, the hardware ignores writes.
                if !self.ram_enabled || ram.len() == 0 { return; }
                let bank = match self.mode {
                    Mbc1Mode::Rom => 0,
                    Mbc1Mode::Ram => self.bank_reg2,
                };
                ram[self.ram_index(ram, bank, addr)] = val;
            }

            _ => panic!("Unimplemented MBC1 write address: {}, value: {}", addr, val),
        }
    }
}

#[derive(Clone, Debug)]
struct Mbc3 {
    rom_bank: u8,
    ram_rtc_enabled: bool,
    ram_rtc_bank: u8,
    rtc: [u8; 5],
}

impl Mbc3 {
    fn read(&self, rom: &[u8], ram: &[u8], addr: u16) -> u8 {
        let mut address = addr as usize;
        match address {
            // ROM Bank 0
            0x0000...0x3FFF => rom[address],

            // Switchable ROM bank
            0x4000...0x7FFF => {
                let bank = self.rom_bank as usize;
                address = (address - ROM_BANK_SIZE) + (bank * ROM_BANK_SIZE);
                rom[address]
            }

            // Switchable RAM bank
            0xA000...0xBFFF => {
                if !self.ram_rtc_enabled {
                    panic!("Attempt to read from external RAM or RTC without RAM and RTC enabled");
                }

                let bank = self.ram_rtc_bank as usize;
                match bank {
                    // Read from RTC values
                    0x8...0xC => {
                        //TODO(wcarlson): Read from RTC values
                        unimplemented!()
                    }

                    // Read from RAM bank
                    _ => {
                        let ram_index = (addr - 0xA000) as usize;
                        ram[ram_index + bank * RAM_BANK_SIZE]
                    }
                }
            }

            _ => panic!("Unimplemented MBC3 read at address: {}", addr),
        }
    }

    fn write(&mut self, ram: &mut [u8], addr: u16, val: u8) {
        match addr {
            // RAM Enable
            0x0000...0x1FFF => {
                self.ram_rtc_enabled = (val & 0b1111) == 0x0A
            }

            // ROM bank
            0x2000...0x3FFF => {
                let mut lower_7_bits = val & 0b0111_1111;
                if lower_7_bits == 0 {
                    lower_7_bits = 1;
                }
                self.rom_bank &= 0b1000_0000;
                self.rom_bank |= lower_7_bits;
            }

            // RAM Bank / RTC register
            0x4000...0x5FFF => {
                self.ram_rtc_bank = val;
            }

            // Latch clock data
            0x6000...0x7FFF => {
                //TODO(wcarlson): latch clock data
            }

            // Switchable RAM bank
            0xA000...0xBFFF => {
                if !self.ram_rtc_enabled {
                    panic!("Attempt to write external RAM or RTC bank without RAM and RTC enabled");
                }

                let bank = self.ram_rtc_bank as usize;
                match bank {
                    // Write to RTC values
                    0x8...0xC => {
                        //TODO(wcarlson): Write to RTC values
                    }

                    // Write to RAM bank
                    _ => {
                        let ram_index = (addr - 0xA000) as usize;
                        ram[ram_index + bank * RAM_BANK_SIZE] = val;
                    }
                }

            }

            _ => panic!("Unimplemented MBC1 write address: {}, value: {}", addr, val),
        }
    }
}