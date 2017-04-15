use cart_header::{CartHeader, MbcType};

const ROM_BANK_SIZE: usize = 0x4000;
const RAM_BANK_SIZE: usize = 0x2000;

#[derive(Clone, Debug)]
enum Mbc {
    None,
    Mbc1(Mbc1),
}

impl Mbc {
    fn new(mbc_type: MbcType) -> Mbc {
        match mbc_type {
            MbcType::NoMbc => Mbc::None,
            MbcType::Mbc1 => Mbc::Mbc1(Mbc1 {
                rom_ram_mode: RomRamMode::Rom,
                ram_enabled: false,
                rom_bank: 1,
                ram_bank: 0,
            }),
            _ => panic!("Unimplemented Mbc Type!"),
        }
    }
}

#[derive(Clone, Debug)]
struct Mbc1 {
    rom_ram_mode: RomRamMode,
    ram_enabled: bool,
    rom_bank: u8,
    ram_bank: u8,
}

impl Mbc1 {
    fn read(&self, rom: &[u8], ram: &[u8], addr: u16) -> u8 {
        let mut address = addr as usize;
        match address {
            // ROM Bank 0
            0x0000...0x3FFF => rom[address],

            // Switchable ROM bank
            0x4000...0x7FFF => {
                let mut bank = self.rom_bank as usize;
                if self.rom_ram_mode == RomRamMode::Ram {
                    bank &= 0b11111
                }
                address = (address - ROM_BANK_SIZE) + (bank * ROM_BANK_SIZE);
                rom[address]
            }

            // Switchable RAM bank
            0xA000...0xBFFF => {
                if !self.ram_enabled {
                    panic!("Attempt to read from external RAM bank without RAM enabled");
                }
                let mut bank = self.ram_bank as usize;
                if self.rom_ram_mode == RomRamMode::Rom {
                    bank = 0;
                }
                let ram_index = (addr - 0xA000) as usize;
                ram[ram_index + bank * RAM_BANK_SIZE]
            }

            _ => panic!("Unimplemented MBC1 read at address: {}", addr),
        }
    }

    fn write(&mut self, ram: &mut [u8], addr: u16, val: u8) {
        match addr {
            // RAM Enable
            0x0000...0x1FFF => {
                self.ram_enabled = (val & 0b1111) == 0x0A
            }

            // ROM bank lower bits write
            0x2000...0x3FFF => {
                let mut lower_5_bits = val & 0b11111;
                if lower_5_bits == 0 {
                    lower_5_bits = 1;
                }
                self.rom_bank &= 0b11100000;
                self.rom_bank |= lower_5_bits;
            }

            // RAM Bank / Upper ROM bank bits write
            0x4000...0x5FFF => {
                match self.rom_ram_mode {
                    RomRamMode::Rom => {
                        let upper_2_bits = val & 0b1100000;
                        self.rom_bank &= 0b10011111;
                        self.rom_bank |= upper_2_bits;
                    }
                    RomRamMode::Ram => {
                        self.ram_bank = val & 0b11;
                    }
                }
            }

            // ROM / RAM Mode
            0x6000...0x7FFF => {
                let mode_val = val & 0b1;
                if mode_val == 0 {
                    self.rom_ram_mode = RomRamMode::Rom;
                }
                else {
                    self.rom_ram_mode = RomRamMode::Ram;
                }
            }

            // Switchable RAM bank
            0xA000...0xBFFF => {
                if !self.ram_enabled {
                    panic!("Attempt to write external RAM bank without RAM enabled");
                }
                let mut bank = self.ram_bank as usize;
                if self.rom_ram_mode == RomRamMode::Rom {
                    bank = 0;
                }
                let ram_index = (addr - 0xA000) as usize;
                ram[ram_index + bank * RAM_BANK_SIZE] = val;
            }

            _ => panic!("Unimplemented MBC1 write address: {}, value: {}", addr, val),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum RomRamMode {
    Rom,
    Ram,
}

#[derive(Clone, Debug)]
pub struct Cart {
    rom: Box<[u8]>,
    ram: Box<[u8]>,
    mbc: Mbc,
    // FIXME: This is only being used in the read_io_port function as a hack to return proper
    // values for the current game
    pub title: String,
}

impl Cart {
    pub fn new(rom: Box<[u8]>, cart_header: &CartHeader) -> Cart {
        Cart {
            rom: rom,
            ram: vec![0; cart_header.ram_size].into_boxed_slice(),
            mbc: Mbc::new(cart_header.cart_type.mbc),
            title: cart_header.title.clone(),
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match self.mbc {
            Mbc::None => self.rom[addr as usize],
            Mbc::Mbc1(ref mbc1) => mbc1.read(&self.rom, &self.ram, addr),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match self.mbc {
            Mbc::None => {},
            Mbc::Mbc1(ref mut mbc1) => mbc1.write(&mut self.ram, addr, val),
        }
    }
}
