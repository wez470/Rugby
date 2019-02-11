use bitflags::bitflags;
use display_derive::Display;
use failure_derive::Fail;

#[derive(Clone, Debug, PartialEq)]
pub struct CartHeader {
    /// The title of the game. At most 16 bytes.
    pub title: Vec<u8>,

    /// Specifies what kind of hardware is inside the cartridge, including memory bank controllers
    /// (MBC), RAM, etc.
    pub cart_type: CartType,

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
pub struct CartType {
    pub mbc: MbcType,
    pub hardware: CartHardware,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MbcType {
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
    Unknown(u8),
}

bitflags! {
    pub struct CartHardware: u8 {
        const RAM           = 1 << 0;
        const TIMER         = 1 << 1;
        const BATTERY       = 1 << 2;
        const RUMBLE        = 1 << 3;
        const ACCELEROMETER = 1 << 4;
    }
}

impl From<u8> for CartType {
    fn from(byte: u8) -> Self {
        use self::MbcType::*;
        let (mbc, hardware) = match byte {
            0x00 => (NoMbc, CartHardware::empty()),
            0x01 => (Mbc1, CartHardware::empty()),
            0x02 => (Mbc1, CartHardware::RAM),
            0x03 => (Mbc1, CartHardware::RAM | CartHardware::BATTERY),
            0x05 => (Mbc2, CartHardware::empty()),
            0x06 => (Mbc2, CartHardware::RAM | CartHardware::BATTERY),
            0x08 => (NoMbc, CartHardware::RAM),
            0x09 => (NoMbc, CartHardware::RAM | CartHardware::BATTERY),
            0x0B => (Mmm01, CartHardware::empty()),
            0x0C => (Mmm01, CartHardware::RAM),
            0x0D => (Mmm01, CartHardware::RAM | CartHardware::BATTERY),
            0x0F => (Mbc3, CartHardware::TIMER | CartHardware::BATTERY),
            0x10 => (Mbc3, CartHardware::RAM | CartHardware::TIMER | CartHardware::BATTERY),
            0x11 => (Mbc3, CartHardware::empty()),
            0x12 => (Mbc3, CartHardware::RAM),
            0x13 => (Mbc3, CartHardware::RAM | CartHardware::BATTERY),
            0x19 => (Mbc5, CartHardware::empty()),
            0x1A => (Mbc5, CartHardware::RAM),
            0x1B => (Mbc5, CartHardware::RAM | CartHardware::BATTERY),
            0x1C => (Mbc5, CartHardware::RUMBLE),
            0x1D => (Mbc5, CartHardware::RAM | CartHardware::RUMBLE),
            0x1E => (Mbc5, CartHardware::RAM | CartHardware::BATTERY | CartHardware::RUMBLE),
            0x20 => (Mbc6, CartHardware::RAM | CartHardware::BATTERY),
            0x22 => (Mbc7, CartHardware::RAM | CartHardware::BATTERY | CartHardware::ACCELEROMETER),
            0xFC => (PocketCamera, CartHardware::empty()),
            0xFD => (BandaiTama5, CartHardware::empty()),
            0xFE => (HuC3, CartHardware::empty()),
            0xFF => (HuC1, CartHardware::RAM | CartHardware::BATTERY),
            n => (Unknown(n), CartHardware::empty()),
        };
        CartType { mbc, hardware }
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
#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum GbcFlag {
    /// A GB game with no GBC support.
    #[display(fmt = "no")]
    Unsupported,

    /// The game has GBC-specific features, but they are optional so it can run in GB mode.
    #[display(fmt = "yes")]
    Supported,

    /// The game requires GBC hardware and does not run in GB mode.
    #[display(fmt = "required")]
    Required,
}

/// Represents the level of Super Game Boy support the game supports.
#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum SgbFlag {
    /// A GB or GBC game with no SGB support.
    #[display(fmt = "no")]
    Unsupported,

    /// The game supports SGB features.
    #[display(fmt = "yes")]
    Supported,
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

        let cart_type = CartType::from(bytes[0x47]);

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