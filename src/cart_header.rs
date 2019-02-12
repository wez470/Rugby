use enumflags2::BitFlags;
use enumflags2_derive::EnumFlags;
use failure_derive::Fail;

#[derive(Clone, Debug, PartialEq)]
pub struct CartHeader {
    /// The title of the game. At most 16 bytes.
    pub title: Vec<u8>,

    /// Specifies what kind of physical cartridge this ROM comes with, e.g. a cartridge with a
    /// memory bank controller.
    pub cart_type: CartType,

    /// Specifies what extra hardware is present in the cartridge.
    pub hardware: BitFlags<CartHardware>,

    /// The size in bytes of the ROM in the cartridge.
    pub rom_size: MemSize,

    /// The size in bytes of the RAM in the cartridge.
    pub ram_size: MemSize,

    /// The level of Game Boy Color support the game has or requires.
    pub gbc_flag: GbcFlag,

    /// The level of Super Game Boy support the game has.
    pub sgb_flag: SgbFlag,

    /// Only present in newer cartridges.
    pub manufacturer_code: Option<String>,

    /// Indicates the company or publisher of the game.
    pub licensee_code: LicenseeCode,

    /// Indicates whether the cartridge is supposed to be sold in Japan or outside Japan.
    pub destination_code: DestinationCode,

    /// Some games have more than one version, and this byte indicates that. Usually zero.
    pub rom_version: u8,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CartType {
    NoMbc,
    Mbc1,
    Mbc2,
    Mbc3,
    Mbc5,
    Mbc6,
    Mbc7,
    Mmm01,
    HuC1,
    HuC3,
    PocketCamera,
    BandaiTama5,
    Unknown(u8),
}

#[derive(Copy, Clone, Debug, EnumFlags)]
#[repr(u8)]
pub enum CartHardware {
    Ram           = 1 << 0,
    Timer         = 1 << 1,
    Battery       = 1 << 2,
    Rumble        = 1 << 3,
    Accelerometer = 1 << 4,
}

impl CartHardware {
    pub fn flags_to_string(flags: BitFlags<CartHardware>) -> String {
        let hardware: Vec<String> = flags.iter().map(|h| format!("{:?}", h)).collect();
        if hardware.is_empty() { String::from("none") } else { hardware.join("+") }
    }
}

/// Represents RAM or ROM size, which are parsed from a single byte in the header according to an
/// arbitrary mapping. We use the `Unknown` variant for values not covered by the mapping.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MemSize {
    /// A size in bytes.
    Bytes(usize),

    /// An unrecognized RAM or ROM size byte from a cartridge header.
    Unknown(u8),
}

impl std::fmt::Display for MemSize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            MemSize::Bytes(b) =>
                if b % 1024 == 0 { write!(f, "{} KiB", b / 1024) } else { write!(f, "{} B", b) },
            MemSize::Unknown(n) =>
                write!(f, "unknown (0x{:02X})", n),
        }
    }
}

/// Represents the level of Game Boy Color support the game supports or requires.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GbcFlag {
    /// A GB game with no GBC support.
    Unsupported,

    /// The game has GBC-specific features, but they are optional so it can run in GB mode.
    Supported,

    /// The game requires GBC hardware and does not run in GB mode.
    Required,
}

impl std::fmt::Display for GbcFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            GbcFlag::Unsupported => "no",
            GbcFlag::Supported => "yes",
            GbcFlag::Required => "required",
        })
    }
}

/// Represents the level of Super Game Boy support the game supports.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SgbFlag {
    /// A GB or GBC game with no SGB support.
    Unsupported,

    /// The game supports SGB features.
    Supported,
}

impl std::fmt::Display for SgbFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            SgbFlag::Unsupported => "no",
            SgbFlag::Supported => "yes",
        })
    }
}

/// Indicates the company or publisher of the game.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LicenseeCode {
    /// A single byte licensee code, used in older cartridges.
    ///
    /// This will never be 0x33 because that value signals that the newer 2-byte code is used
    /// instead.
    Old(u8),

    /// A two character ASCII licensee code, used in newer cartridges.
    New([u8; 2]),
}

/// Indicates whether the cartridge is supposed to be sold in Japan or outside Japan.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DestinationCode {
    Japan,
    International,
    Invalid(u8),
}

#[derive(Clone, Debug, Fail, PartialEq)]
pub enum HeaderParseError {
    #[fail(display = "manufacturer code was not valid UTF-8: {:?}", _0)]
    InvalidManufacturerCodeUtf8(Vec<u8>),

    #[fail(display = "cartridge ROM has length {} which is too short to contain a header", _0)]
    RomTooShort(usize),
}

impl CartHeader {
    /// Parse the cartridge header from the given ROM. The header is in the range 0x100..0x150, so
    /// the input slice must be at least large enough to contain that.
    pub fn from_rom(rom: &[u8]) -> Result<Self, HeaderParseError> {
        let bytes = rom.get(0x100..0x150).ok_or(HeaderParseError::RomTooShort(rom.len()))?;

        // The last byte of the title is used to determine if this is a Game Boy Color game. This
        // GBC flag uses byts which aren't valid ASCII, so we know this isn't actually part of the
        // title if the flag is set.
        let gbc_flag = match bytes[0x43] {
            0x80 => GbcFlag::Supported,
            0xC0 => GbcFlag::Required,
            _ => GbcFlag::Unsupported,
        };

        let (mut title_slice, manufacturer_code) = if gbc_flag == GbcFlag::Unsupported {
            // For GB-only games, all 16 bytes in 0x34..0x44 may be used for the game title.
            (&bytes[0x34..0x44], None)
        } else {
            // For GBC games, the first 11 bytes are used for the game title and the next 4 bytes
            // are used for the manufacturer code.
            let manufacturer_code = String::from_utf8(bytes[0x3F..0x43].to_vec())
                .map_err(|e| HeaderParseError::InvalidManufacturerCodeUtf8(e.into_bytes()))?;
            (&bytes[0x34..0x3F], Some(manufacturer_code))
        };

        // Ignore trailing null bytes.
        if let Some(end) = title_slice.iter().position(|&x| x == 0) {
            title_slice = &title_slice[..end];
        };

        let title = title_slice.to_vec();

        let sgb_flag = match bytes[0x46] {
            0x03 => SgbFlag::Supported,
            _ => SgbFlag::Unsupported,
        };

        let licensee_code = if sgb_flag == SgbFlag::Unsupported {
            LicenseeCode::Old(bytes[0x4B])
        } else {
            LicenseeCode::New([bytes[0x44], bytes[0x45]])
        };

        let cart_type = match bytes[0x47] {
            0x00 | 0x08...0x09 => CartType::NoMbc,
            0x01...0x03 => CartType::Mbc1,
            0x05...0x06 => CartType::Mbc2,
            0x0B...0x0D => CartType::Mmm01,
            0x0F...0x13 => CartType::Mbc3,
            0x19...0x1E => CartType::Mbc5,
            0x20 => CartType::Mbc6,
            0x22 => CartType::Mbc7,
            0xFC => CartType::PocketCamera,
            0xFD => CartType::BandaiTama5,
            0xFE => CartType::HuC3,
            0xFF => CartType::HuC1,
            n => CartType::Unknown(n),
        };

        use self::CartHardware::*;
        let mut hardware = BitFlags::empty();
        match bytes[0x47] {
            0x02 | 0x08 | 0x0C | 0x12 | 0x1A => hardware |= Ram,
            0x03 | 0x06 | 0x09 | 0x0D | 0x13 | 0x1B | 0x20 | 0xFF => hardware |= Ram | Battery,
            0x0F => hardware |= Timer | Battery,
            0x10 => hardware |= Ram | Timer | Battery,
            0x1C => hardware |= Rumble,
            0x1D => hardware |= Ram | Rumble,
            0x1E => hardware |= Ram | Battery | Rumble,
            0x22 => hardware |= Ram | Battery | Accelerometer,
            _ => {}
        };

        let rom_size = match bytes[0x48] {
            n @ 0x00...0x08 => MemSize::Bytes((32 * 1024) << n),
            n => MemSize::Unknown(n),
        };

        let ram_size = match bytes[0x49] {
            0x00 => MemSize::Bytes(0),
            0x01 => MemSize::Bytes(2 * 1024),
            0x02 => MemSize::Bytes(8 * 1024),
            0x03 => MemSize::Bytes(32 * 1024),
            0x04 => MemSize::Bytes(128 * 1024),
            0x05 => MemSize::Bytes(64 * 1024),
            n => MemSize::Unknown(n),
        };

        let destination_code = match bytes[0x4A] {
            0x00 => DestinationCode::Japan,
            0x01 => DestinationCode::International,
            n => DestinationCode::Invalid(n),
        };

        let rom_version = bytes[0x4C];

        Ok(CartHeader {
            title,
            cart_type,
            hardware,
            rom_size,
            ram_size,
            gbc_flag,
            sgb_flag,
            manufacturer_code,
            licensee_code,
            destination_code,
            rom_version,
        })
    }
}