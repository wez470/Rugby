use crate::cart_header::{CartHeader, CartType, MemSize};
use failure_derive::Fail;
use log::warn;

const ROM_BANK_SIZE: usize = 0x4000;
const RAM_BANK_SIZE: usize = 0x2000;

#[derive(Clone, Debug)]
pub struct CartConfig {
    pub cart_type: CartType,
    pub rom_size: usize,
    pub ram_size: usize,
}

impl CartConfig {
    pub fn from_cart_header(cart_header: &CartHeader) -> Result<Self, CartError> {
        Ok(CartConfig {
            cart_type: cart_header.cart_type,
            rom_size: match cart_header.rom_size {
                MemSize::Bytes(b) => b,
                MemSize::Unknown(_) => return Err(CartError::RomSizeUnknown),
            },
            ram_size: match cart_header.rom_size {
                MemSize::Bytes(b) => b,
                MemSize::Unknown(_) => return Err(CartError::RamSizeUnknown),
            },
        })
    }
}

#[derive(Clone, Debug)]
pub enum Cart {
    NoMbc(NoMbc),
    Mbc1(Mbc1),
    Mbc3(Mbc3),
}

#[derive(Clone, Debug, Fail)]
pub enum CartError {
    #[fail(display = "provided RAM is {} bytes but the cartridge requires {}", actual, expected)]
    ProvidedRamWrongSize {
        expected: usize,
        actual: usize,
    },

    #[fail(display = "ROM size unknown in cartridge header")]
    RomSizeUnknown,

    #[fail(display = "RAM size unknown in cartridge header")]
    RamSizeUnknown,
}

impl Cart {
    pub fn new(
        rom: Box<[u8]>,
        ram_opt: Option<Box<[u8]>>,
        config: &CartConfig,
    ) -> Result<Cart, CartError> {
        let ram = match ram_opt {
            Some(ram) => {
                if ram.len() != config.ram_size {
                    return Err(CartError::ProvidedRamWrongSize {
                        expected: config.ram_size,
                        actual: ram.len(),
                    });
                }
                ram
            }
            None => vec![0; config.ram_size].into_boxed_slice(),
        };

        Ok(match config.cart_type {
            CartType::NoMbc => Cart::NoMbc(NoMbc::new(rom, ram)),
            CartType::Mbc1 => Cart::Mbc1(Mbc1::new(rom, ram)),
            CartType::Mbc3 => Cart::Mbc3(Mbc3::new(rom, ram)),
            _ => panic!("Unimplemented Mbc Type!"),
        })
    }

    pub fn read(&self, addr: u16) -> u8 {
        match self {
            Cart::NoMbc(nombc) => nombc.read(addr),
            Cart::Mbc1(mbc1) => mbc1.read(addr),
            Cart::Mbc3(mbc3) => mbc3.read(addr),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match self {
            Cart::NoMbc(nombc) => nombc.write(addr, val),
            Cart::Mbc1(mbc1) => mbc1.write(addr, val),
            Cart::Mbc3(mbc3) => mbc3.write(addr, val),
        }
    }

    #[cfg(test)] // Silence "never used" warning.
    pub fn rom(&self) -> &[u8] {
        match self {
            Cart::NoMbc(nombc) => &nombc.rom,
            Cart::Mbc1(mbc1) => &mbc1.rom,
            Cart::Mbc3(mbc3) => &mbc3.rom,
        }
    }

    pub fn ram(&self) -> &[u8] {
        match self {
            Cart::NoMbc(nombc) => &nombc.ram,
            Cart::Mbc1(mbc1) => &mbc1.ram,
            Cart::Mbc3(mbc3) => &mbc3.ram,
        }
    }
}

#[derive(Clone, Debug)]
pub struct NoMbc {
    rom: Box<[u8]>,
    ram: Box<[u8]>,
}

impl NoMbc {
    fn new(rom: Box<[u8]>, ram: Box<[u8]>) -> Self {
        Self { rom, ram }
    }

    fn read(&self, addr: u16) -> u8 {
        match addr {
            // ROM
            0x0000...0x7FFF => self.rom[addr as usize],

            // RAM
            0xA000...0xBFFF => {
                let ram_index = (addr - 0xA000) as usize;
                // If the RAM is not present, the hardware returns all bits set.
                *self.ram.get(ram_index).unwrap_or(&0xFF)
            }

            _ => panic!("Unimplemented Mbc::None read address: {}", addr),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // ROM: Writes are ignored.
            0x0000...0x7FFF => {}

            // RAM
            0xA000...0xBFFF => {
                let ram_index = (addr - 0xA000) as usize;
                // If the RAM is not present, the hardware ignores writes.
                if let Some(p) = self.ram.get_mut(ram_index) {
                    *p = val;
                }
            }

            _ => panic!("Unimplemented Mbc::None read address: {}", addr),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Mbc1 {
    rom: Box<[u8]>,
    ram: Box<[u8]>,
    mode: Mbc1Mode,
    ram_enabled: bool,
    bank_reg1: u8,
    bank_reg2: u8,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mbc1Mode {
    Rom,
    Ram,
}

impl Mbc1 {
    fn new(rom: Box<[u8]>, ram: Box<[u8]>) -> Self {
        Self { rom, ram, mode: Mbc1Mode::Rom, ram_enabled: false, bank_reg1: 1, bank_reg2: 0 }
    }

    // TODO(solson): Combine with ram_index.
    fn rom_index(&self, bank: u8, addr: u16) -> usize {
        let bank_base = bank as usize * ROM_BANK_SIZE;
        let addr_in_bank = addr as usize & (ROM_BANK_SIZE - 1);
        (bank_base | addr_in_bank) & (self.rom.len() - 1)
    }

    fn ram_index(&self, bank: u8, addr: u16) -> usize {
        let bank_base = bank as usize * RAM_BANK_SIZE;
        let addr_in_bank = addr as usize & (RAM_BANK_SIZE - 1);
        (bank_base | addr_in_bank) & (self.ram.len() - 1)
    }

    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x3FFF => {
                let bank = match self.mode {
                    Mbc1Mode::Rom => 0,
                    Mbc1Mode::Ram => self.bank_reg2 << 5,
                };
                self.rom[self.rom_index(bank, addr)]
            }

            0x4000...0x7FFF => {
                let bank = self.bank_reg2 << 5 | self.bank_reg1;
                self.rom[self.rom_index(bank, addr)]
            }

            0xA000...0xBFFF => {
                // When RAM is disabled, the hardware returns all bits set.
                if !self.ram_enabled || self.ram.len() == 0 { return 0xFF; }
                let bank = match self.mode {
                    Mbc1Mode::Rom => 0,
                    Mbc1Mode::Ram => self.bank_reg2,
                };
                self.ram[self.ram_index(bank, addr)]
            }

            _ => panic!("Unimplemented MBC1 read at address: {}", addr),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
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
                if !self.ram_enabled || self.ram.len() == 0 { return; }
                let bank = match self.mode {
                    Mbc1Mode::Rom => 0,
                    Mbc1Mode::Ram => self.bank_reg2,
                };
                self.ram[self.ram_index(bank, addr)] = val;
            }

            _ => panic!("Unimplemented MBC1 write address: {}, value: {}", addr, val),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Mbc3 {
    rom: Box<[u8]>,
    ram: Box<[u8]>,
    rom_bank: u8,
    ram_rtc_enabled: bool,
    ram_rtc_bank: u8,
    rtc: [u8; 5],
}

impl Mbc3 {
    fn new(rom: Box<[u8]>, ram: Box<[u8]>) -> Self {
        Self { rom, ram, rom_bank: 1, ram_rtc_enabled: false, ram_rtc_bank: 0, rtc: [0; 5] }
    }

    fn read(&self, addr: u16) -> u8 {
        let mut address = addr as usize;
        match address {
            // ROM Bank 0
            0x0000...0x3FFF => self.rom[address],

            // Switchable ROM bank
            0x4000...0x7FFF => {
                let bank = self.rom_bank as usize;
                address = (address - ROM_BANK_SIZE) + (bank * ROM_BANK_SIZE);
                self.rom[address]
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
                        self.ram[ram_index + bank * RAM_BANK_SIZE]
                    }
                }
            }

            _ => panic!("Unimplemented MBC3 read at address: {}", addr),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
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
                warn!("unimplemented: write to MBC3 RTC latch register");
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
                        warn!("unimplemented: write to MBC3 RTC data");
                    }

                    // Write to RAM bank
                    _ => {
                        let ram_index = (addr - 0xA000) as usize;
                        self.ram[ram_index + bank * RAM_BANK_SIZE] = val;
                    }
                }
            }

            _ => panic!("Unimplemented MBC1 write address: {}, value: {}", addr, val),
        }
    }
}