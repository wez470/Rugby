use cart_header::{CartHeader, MbcType};

const ROM_BANK_SIZE: usize = 0x4000;

#[derive(Clone, Debug)]
enum Mbc {
    None,
    Mbc1 {
        rom_ram_mode: RomRamMode,
        ram_enabled: bool,
        rom_bank: u8,
        ram_bank: u8,
    },
}

impl Mbc {
    fn new(mbc_type: MbcType) -> Mbc {
        match mbc_type {
            MbcType::NoMbc => Mbc::None,
            MbcType::Mbc1 => Mbc::Mbc1 {
                rom_ram_mode: RomRamMode::Rom,
                ram_enabled: false,
                rom_bank: 1,
                ram_bank: 0,
            },
            _ => panic!("Unimplemented Mbc Type!"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
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
            Mbc::Mbc1 {rom_ram_mode, ram_enabled, rom_bank, ram_bank} => self.read_mbc_1(addr, rom_bank),
        }
    }

    fn read_mbc_1(&self, addr: u16, rom_bank: u8) -> u8 {
        let mut address = addr as usize;
        if is_switchable_rom_bank(addr) {
            address = (address - ROM_BANK_SIZE) + (rom_bank as usize * ROM_BANK_SIZE);
        }
        self.rom[address]
    }

}

fn is_switchable_rom_bank(addr: u16) -> bool {
    return addr >= 0x4000 && addr < 0x8000
}
