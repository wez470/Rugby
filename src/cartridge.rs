use std::fmt;

/// The cartridge header takes up the range from 0x100 to 0x150 (exclusive).
const HEADER_SIZE: usize = 0x150 - 0x100;

#[derive(Clone, Debug, PartialEq)]
struct CartHeader {
    /// The title of the game. At most 16 bytes.
    title: String,

    /// Specifies what kind of hardware is inside the cartridge, including memory bank controllers
    /// (MBC), RAM, etc.
    cart_type: CartType,

    /// The size in bytes of the ROM in the cartridge.
    rom_size: usize,

    /// The size in bytes of the RAM in the cartridge.
    ram_size: usize,

    /// The level of Game Boy Color support the game has or requires.
    gbc_flag: GbcFlag,

    /// The level of Super Game Boy support the game has.
    sgb_flag: SgbFlag,

    /// Only present in newer cartridges.
    manufacturer_code: Option<String>,

    /// Indicates the company or publisher of the game.
    licensee_code: LicenseeCode,

    /// Indicates whether the cartridge is supposed to be sold in Japan or outside Japan.
    destination_code: DestinationCode,

    /// Some games have more than one version, and this byte indicates that. Usually zero.
    rom_version: u8,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct CartType {
    mbc: MbcType,
    hardware: CartHardware,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum MbcType {
    /// Plain ROM without an MBC.
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
}

bitflags! {
    flags CartHardware: u8 {
        const NONE          = 0,
        const RAM           = 1 << 0,
        const TIMER         = 1 << 1,
        const BATTERY       = 1 << 2,
        const RUMBLE        = 1 << 3,
        const ACCELEROMETER = 1 << 4,
    }
}

/// Represents the level of Game Boy Color support the game supports or requires.
#[derive(Clone, Copy, Debug, PartialEq)]
enum GbcFlag {
    /// A GB game with no GBC support.
    Unsupported,

    /// The game has GBC-specific features, but they are optional so it can run in GB mode.
    Supported,

    /// The game requires GBC hardware and does not run in GB mode.
    Required,
}

/// Represents the level of Super Game Boy support the game supports.
#[derive(Clone, Copy, Debug, PartialEq)]
enum SgbFlag {
    /// A GB or GBC game with no SGB support.
    Unsupported,

    /// The game supports SGB features.
    Supported,
}

/// Indicates the company or publisher of the game.
#[derive(Clone, Copy, Debug, PartialEq)]
enum LicenseeCode {
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
enum DestinationCode {
    Japan,
    International,
}

#[derive(Clone, Debug, PartialEq)]
enum HeaderParseError {
    InvalidCartType(u8),
    InvalidDestinationCode(u8),
    InvalidHeaderSize(usize),
    InvalidManufacturerCodeUtf8(Vec<u8>),
    InvalidRamSize(u8),
    InvalidRomSize(u8),
    InvalidTitleUtf8(Vec<u8>),
}

impl CartHeader {
    /// Parse a cartridge header out of a cartridge. This must be passed exactly the range from
    /// 0x100 to 0x14F (inclusive) from the cartridge data, which is 0x50 (80) bytes.
    fn from_header_bytes(bytes: &[u8]) -> Result<Self, HeaderParseError> {
        if bytes.len() != HEADER_SIZE {
            return Err(HeaderParseError::InvalidHeaderSize(bytes.len()));
        }

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
            let manufacturer_code = String::from_utf8(bytes[0x40..0x43].to_vec())
                .map_err(|e| HeaderParseError::InvalidManufacturerCodeUtf8(e.into_bytes()))?;
            (&bytes[0x34..0x40], Some(manufacturer_code))
        };

        // Ignore trailing null bytes.
        if let Some(end) = title_slice.iter().position(|&x| x == 0) {
            title_slice = &title_slice[..end];
        };

        // Parse the title as UTF-8.
        let title = String::from_utf8(title_slice.to_vec())
            .map_err(|e| HeaderParseError::InvalidTitleUtf8(e.into_bytes()))?;

        let sgb_flag = match bytes[0x46] {
            0x03 => SgbFlag::Supported,
            _ => SgbFlag::Unsupported,
        };

        let licensee_code = if sgb_flag == SgbFlag::Unsupported {
            LicenseeCode::Old(bytes[0x4B])
        } else {
            LicenseeCode::New([bytes[0x44], bytes[0x45]])
        };

        let cart_type = CartType::from_header_byte(bytes[47])?;

        let rom_size = match bytes[0x48] {
            n @ 0x00...0x08 => (32 * 1024) << n, // 32 KB << n
            n => return Err(HeaderParseError::InvalidRomSize(n)),
        };

        // RAM size in KB.
        let ram_size_kb = match bytes[0x49] {
            0x00 => 0,
            0x01 => 2,
            0x02 => 8,
            0x03 => 32,
            0x04 => 128,
            0x05 => 64,
            n => return Err(HeaderParseError::InvalidRamSize(n)),
        };

        let destination_code = match bytes[0x4A] {
            0x00 => DestinationCode::Japan,
            0x01 => DestinationCode::International,
            n => return Err(HeaderParseError::InvalidDestinationCode(n)),
        };

        let rom_version = bytes[0x4C];

        Ok(CartHeader {
            title: title,
            cart_type: cart_type,
            rom_size: rom_size,
            ram_size: ram_size_kb * 1024,
            gbc_flag: gbc_flag,
            sgb_flag: sgb_flag,
            manufacturer_code: manufacturer_code,
            licensee_code: licensee_code,
            destination_code: destination_code,
            rom_version: rom_version,
        })
    }
}

impl CartType {
    fn from_header_byte(b: u8) -> Result<Self, HeaderParseError> {
        use self::MbcType::*;

        let (mbc, flags) = match b {
            0x00 => (NoMbc, NONE),
            0x01 => (Mbc1, NONE),
            0x02 => (Mbc1, RAM),
            0x03 => (Mbc1, RAM | BATTERY),
            0x05 => (Mbc2, NONE),
            0x06 => (Mbc2, RAM | BATTERY),
            0x08 => (NoMbc, RAM),
            0x09 => (NoMbc, RAM | BATTERY),
            0x0B => (Mmm01, NONE),
            0x0C => (Mmm01, RAM),
            0x0D => (Mmm01, RAM | BATTERY),
            0x0F => (Mbc3, TIMER | BATTERY),
            0x10 => (Mbc3, RAM | TIMER | BATTERY),
            0x11 => (Mbc3, NONE),
            0x12 => (Mbc3, RAM),
            0x13 => (Mbc3, RAM | BATTERY),
            0x19 => (Mbc5, NONE),
            0x1A => (Mbc5, RAM),
            0x1B => (Mbc5, RAM | BATTERY),
            0x1C => (Mbc5, RUMBLE),
            0x1D => (Mbc5, RAM | RUMBLE),
            0x1E => (Mbc5, RAM | BATTERY | RUMBLE),
            0x20 => (Mbc6, RAM | BATTERY),
            0x22 => (Mbc7, RAM | BATTERY | ACCELEROMETER),
            0xFC => (PocketCamera, NONE),
            0xFD => (BandaiTama5, NONE),
            0xFE => (HuC3, NONE),
            0xFF => (HuC1, RAM | BATTERY),
            _ => return Err(HeaderParseError::InvalidCartType(b)),
        };

        Ok(CartType { mbc: mbc, hardware: flags })
    }
}

impl fmt::Display for HeaderParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::HeaderParseError::*;
        match *self {
            InvalidCartType(b) =>
                writeln!(f, "invalid \"cartridge type\" specification: {}", b),
            InvalidDestinationCode(b) =>
                writeln!(f, "invalid \"destination code\" specification: {}", b),
            InvalidRamSize(b) =>
                writeln!(f, "invalid \"RAM size\" specification: {}", b),
            InvalidRomSize(b) =>
                writeln!(f, "invalid \"ROM size\" specification: {}", b),
            InvalidHeaderSize(n) =>
                writeln!(f, "invalid cartridge header size: {}, should be {}", n, HEADER_SIZE),
            InvalidManufacturerCodeUtf8(ref bytes) =>
                writeln!(f, "manufacturer code was not valid UTF-8: {:?}", bytes),
            InvalidTitleUtf8(ref bytes) =>
                writeln!(f, "cartridge title was not valid UTF-8: {:?}", bytes),
        }
    }
}
