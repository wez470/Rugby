use crate::cart_header::{CartHeader, CartType, MemSize};
use failure_derive::Fail;
use log::warn;

const ROM_BANK_SIZE: usize = 0x4000;
const RAM_BANK_SIZE: usize = 0x2000;

#[derive(Clone, Debug)]
pub struct CartConfig {
    pub title: Vec<u8>,
    pub cart_type: CartType,
    pub rom_size: usize,
    pub ram_size: usize,
}

impl CartConfig {
    pub fn from_cart_header(cart_header: &CartHeader) -> Result<Self, CartError> {
        Ok(CartConfig {
            title: cart_header.title.clone(),
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
pub struct Cart {
    pub title: Vec<u8>,
    pub cart_data: CartTypeData,
}

#[derive(Clone, Debug)]
pub enum CartTypeData {
    NoMbc(NoMbc),
    Mbc1(Mbc1),
    Mbc3(Mbc3),
    Mbc5(Mbc5),
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

        let cart_data = match config.cart_type {
            CartType::NoMbc => CartTypeData::NoMbc(NoMbc::new(rom, ram)),
            CartType::Mbc1 => CartTypeData::Mbc1(Mbc1::new(rom, ram)),
            CartType::Mbc3 => CartTypeData::Mbc3(Mbc3::new(rom, ram)),
            CartType::Mbc5 => CartTypeData::Mbc5(Mbc5::new(rom, ram)),
            _ => panic!("Unimplemented Mbc Type!"),
        };
        Ok(Cart{title: config.title.clone(), cart_data})
    }

    pub fn read(&self, addr: u16) -> u8 {
        match &self.cart_data {
            CartTypeData::NoMbc(nombc) => nombc.read(addr),
            CartTypeData::Mbc1(mbc1) => mbc1.read(addr),
            CartTypeData::Mbc3(mbc3) => mbc3.read(addr),
            CartTypeData::Mbc5(mbc5) => mbc5.read(addr),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match &mut self.cart_data {
            CartTypeData::NoMbc(nombc) => nombc.write(addr, val),
            CartTypeData::Mbc1(mbc1) => mbc1.write(addr, val),
            CartTypeData::Mbc3(mbc3) => mbc3.write(addr, val),
            CartTypeData::Mbc5(mbc5) => mbc5.write(addr, val),
        }
    }

    #[cfg(test)] // Silence "never used" warning.
    pub fn rom(&self) -> &[u8] {
        match &self.cart_data {
            CartTypeData::NoMbc(nombc) => &nombc.rom,
            CartTypeData::Mbc1(mbc1) => &mbc1.rom,
            CartTypeData::Mbc3(mbc3) => &mbc3.rom,
            CartTypeData::Mbc5(mbc5) => &mbc5.rom,
        }
    }

    pub fn ram(&self) -> &[u8] {
        match &self.cart_data {
            CartTypeData::NoMbc(nombc) => &nombc.ram,
            CartTypeData::Mbc1(mbc1) => &mbc1.ram,
            CartTypeData::Mbc3(mbc3) => &mbc3.ram,
            CartTypeData::Mbc5(mbc5) => &mbc5.ram,
        }
    }

    pub fn set_ram(&mut self, ram: Box<[u8]>) {
        match &mut self.cart_data {
            CartTypeData::NoMbc(nombc) => nombc.ram = ram,
            CartTypeData::Mbc1(mbc1) => mbc1.ram = ram,
            CartTypeData::Mbc3(mbc3) => mbc3.ram = ram,
            CartTypeData::Mbc5(mbc5) => mbc5.ram = ram,
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
    mode: MbcMode,
    ram_enabled: bool,
    bank_reg1: u8,
    bank_reg2: u8,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MbcMode {
    Rom,
    Ram,
}

impl Mbc1 {
    fn new(rom: Box<[u8]>, ram: Box<[u8]>) -> Self {
        Self { rom, ram, mode: MbcMode::Rom, ram_enabled: false, bank_reg1: 1, bank_reg2: 0 }
    }

    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x3FFF => {
                let bank = match self.mode {
                    MbcMode::Rom => 0,
                    MbcMode::Ram => self.bank_reg2 << 5,
                };
                get_rom(&self.rom, bank as u16, addr)
            }

            0x4000...0x7FFF => {
                let bank = self.bank_reg2 << 5 | self.bank_reg1;
                get_rom(&self.rom, bank as u16, addr)
            }

            0xA000...0xBFFF => {
                // When RAM is disabled, the hardware returns all bits set.
                if !self.ram_enabled || self.ram.len() == 0 { return 0xFF; }
                let bank = match self.mode {
                    MbcMode::Rom => 0,
                    MbcMode::Ram => self.bank_reg2,
                };
                get_ram(&self.ram, bank as u16, addr)
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
                self.mode = if val & 1 == 0 { MbcMode::Rom } else { MbcMode::Ram };
            }

            // Switchable RAM bank
            0xA000...0xBFFF => {
                // When RAM is disabled, the hardware ignores writes.
                if !self.ram_enabled || self.ram.len() == 0 { return; }
                let bank = match self.mode {
                    MbcMode::Rom => 0,
                    MbcMode::Ram => self.bank_reg2,
                };
                set_ram(&mut self.ram, bank as u16, addr, val);
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
        match addr {
            // ROM Bank 0
            0x0000...0x3FFF => get_rom(&self.rom, 0, addr),

            // Switchable ROM bank
            0x4000...0x7FFF => get_rom(&self.rom, self.rom_bank as u16, addr),

            // Switchable RAM bank
            0xA000...0xBFFF => {
                // When RAM is disabled, the hardware returns all bits set.
                if !self.ram_rtc_enabled { return 0xFF; }

                let bank = self.ram_rtc_bank as u16;
                match bank {
                    // Read from RTC values
                    0x8...0xC => {
                        //TODO(wcarlson): Read from RTC values
                        unimplemented!()
                    }

                    // Read from RAM bank
                    _ => get_ram(&self.ram, bank, addr),
                }
            }

            _ => panic!("Unimplemented MBC3 read at address: {}", addr),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // RAM Enable
            0x0000...0x1FFF => {
                self.ram_rtc_enabled = (val & 0b1111) == 0b1010;
            }

            // ROM bank
            0x2000...0x3FFF => {
                self.rom_bank = val & 0b0111_1111;
                if self.rom_bank == 0 {
                    self.rom_bank = 1;
                }
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
                // When RAM is disabled, the hardware ignores writes.
                if !self.ram_rtc_enabled { return; }

                let bank = self.ram_rtc_bank as u16;
                match bank {
                    // Write to RTC values
                    0x8...0xC => {
                        warn!("unimplemented: write to MBC3 RTC data");
                    }

                    // Write to RAM bank
                    _ => {
                        // let ram_index = (addr - 0xA000) as usize;
                        // self.ram[ram_index + bank * RAM_BANK_SIZE] = val;
                        set_ram(&mut self.ram, bank, addr, val);
                    }
                }
            }

            _ => panic!("Unimplemented MBC1 write address: {}, value: {}", addr, val),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Mbc5 {
    rom: Box<[u8]>,
    ram: Box<[u8]>,
    ram_enabled: bool,
    rom_bank_reg1: u8,
    rom_bank_reg2: u8,
    ram_bank_reg: u8,
}

impl Mbc5 {
    fn new(rom: Box<[u8]>, ram: Box<[u8]>) -> Self {
        Self {
            rom,
            ram,
            ram_enabled: false,
            rom_bank_reg1: 0,
            rom_bank_reg2: 0,
            ram_bank_reg: 0,
        }
    }

    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x3FFF => {
                get_rom(&self.rom, 0, addr)
            }

            0x4000...0x7FFF => {
                let bank = u16::from_le_bytes([self.rom_bank_reg1, self.rom_bank_reg2]);
                get_rom(&self.rom, bank, addr)
            }

            0xA000...0xBFFF => {
                // When RAM is disabled, the hardware returns all bits set.
                if !self.ram_enabled || self.ram.len() == 0 { return 0xFF; }
                get_ram(&self.ram, self.ram_bank_reg as u16, addr)
            }

            _ => panic!("Unimplemented MBC5 read at address: {}", addr),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            // RAM Enable
            0x0000...0x1FFF => {
                self.ram_enabled = (val & 0b1111) == 0b1010;
            }

            // ROM bank lower bits write
            0x2000...0x2FFF => {
                self.rom_bank_reg1 = val;
            }

            // ROM bank higher bit write
            0x3000...0x3FFF => {
                self.rom_bank_reg2 = val & 0b0001;
            }

            // RAM bank write
            0x4000...0x5FFF => {
                self.ram_bank_reg = val & 0b1111;
            }

            // Switchable RAM bank
            0xA000...0xBFFF => {
                // When RAM is disabled, the hardware ignores writes.
                if !self.ram_enabled || self.ram.len() == 0 { return; }
                set_ram(&mut self.ram, self.ram_bank_reg as u16, addr, val);
            }

            _ => panic!("Unimplemented MBC5 write address: {}, value: {}", addr, val),
        }
    }
}

fn bank_index(bank: u16, addr: u16, bank_size: usize, total_size: usize) -> usize {
    let bank_base = bank as usize * bank_size;
    let addr_in_bank = addr as usize & (bank_size - 1);
    (bank_base | addr_in_bank) & (total_size - 1)
}

fn get_rom(rom: &[u8], bank: u16, addr: u16) -> u8 {
    rom[bank_index(bank, addr, ROM_BANK_SIZE, rom.len())]
}

fn get_ram(ram: &[u8], bank: u16, addr: u16) -> u8 {
    ram[bank_index(bank, addr, RAM_BANK_SIZE, ram.len())]
}

fn set_ram(ram: &mut [u8], bank: u16, addr: u16, val: u8) {
    ram[bank_index(bank, addr, RAM_BANK_SIZE, ram.len())] = val;
}
