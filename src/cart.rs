use cart_header::{CartHeader, MbcType};

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
    // TODO: Make methods to access this and remove pub access
    pub rom: Box<[u8]>,
    ram: Box<[u8]>,
    mbc: Mbc,
}

impl Cart {
    pub fn new(rom: Box<[u8]>, cart_header: &CartHeader) -> Cart {
        Cart {
            rom: rom,
            ram: vec![0; cart_header.ram_size].into_boxed_slice(),
            mbc: Mbc::new(cart_header.cart_type.mbc),
        }
    }
}
