//! Module for dealing with debug symbol information from the WLA DX assembler.
//! 
//! See [the WLA DX documentation](https://wla-dx.readthedocs.io/en/latest/symbols.html) for
//! details on the format this module parses.

use lazy_static::lazy_static;
use log::warn;
use regex::Regex;
use std::collections::{BTreeMap, HashMap};
use std::io::BufRead;

#[derive(Clone, Debug)]
pub struct WlaSymbols {
    // TODO(solson): Figure out what to do if two labels have the same address.
    pub labels: BTreeMap<RomAddr, String>,

    // TODO(solson): Figure out what happens if a symbol name is reused or if two symbols have the
    // same address.
    pub symbols: HashMap<String, RomAddr>,

    pub breakpoints: Vec<RomAddr>,
    pub definitions: HashMap<String, usize>,
    pub source_files: HashMap<usize, SourceFile>,
    pub rom_crc32: Option<u32>,
    pub addr_to_line: BTreeMap<RomAddr, SourceLine>,
}

/// Represents an address and the ROM bank it comes from.
// TODO(solson): Figure out how to interpret these, e.g. "01:c003 delay_a_20_cycles" from
// mem_timing/sources/01-read_timing.s.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RomAddr {
    // NOTE: bank must come before addr for the PartialOrd derive.
    pub bank: u8,
    pub addr: u16,
}

impl std::fmt::Display for RomAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:02X}:{:04X}", self.bank, self.addr)
    }
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub crc32: u32,
    pub path: String,
}

#[derive(Clone, Debug)]
pub struct SourceLine {
    pub source_file_index: usize,
    pub line: usize,
}

#[derive(Debug)]
pub enum WlaSymbolsError {
    TextBeforeFirstSection(String),
    NonmatchingLine {
        actual: String,
        expected_regex: String,
    },
    MultipleRomChecksums,
    Io(std::io::Error),
}

impl std::convert::From<std::io::Error> for WlaSymbolsError {
    fn from(e: std::io::Error) -> Self {
        WlaSymbolsError::Io(e)
    }
}

enum Section {
    BeforeFirst,
    Unknown,
    Labels,
    Definitions,
    Breakpoints,
    Symbols,
    SourceFiles,
    RomChecksum,
    AddrLineMappings,
}

lazy_static! {
    static ref SECTION_REGEX: Regex =
        Regex::new(r"\[(.+)\]").unwrap();

    static ref LABEL_REGEX: Regex =
        Regex::new(r"([[:xdigit:]]{2}):([[:xdigit:]]{4}) (.*)").unwrap();

    static ref SYMBOL_REGEX: Regex =
        Regex::new(r"([[:xdigit:]]{2}):([[:xdigit:]]{4}) (.*)").unwrap();

    static ref BREAKPOINT_REGEX: Regex =
        Regex::new(r"([[:xdigit:]]{2}):([[:xdigit:]]{4})").unwrap();

    static ref DEFINITION_REGEX: Regex =
        Regex::new(r"([[:xdigit:]]{8}) (.*)").unwrap();

    static ref SOURCE_FILE_REGEX: Regex =
        Regex::new(r"([[:xdigit:]]{4}) ([[:xdigit:]]{8}) (.*)").unwrap();

    static ref ROM_CHECKSUM_REGEX: Regex =
        Regex::new(r"([[:xdigit:]]{8})").unwrap();

    static ref ADDR_LINE_MAPPING_REGEX: Regex =
        Regex::new(r"([[:xdigit:]]{2}):([[:xdigit:]]{4}) ([[:xdigit:]]{4}):([[:xdigit:]]{8})")
            .unwrap();
}

impl WlaSymbols {
    fn new() -> Self {
        Self {
            labels: BTreeMap::new(),
            symbols: HashMap::new(),
            breakpoints: Vec::new(),
            definitions: HashMap::new(),
            source_files: HashMap::new(),
            rom_crc32: None,
            addr_to_line: BTreeMap::new(),
        } 
    }

    pub fn parse(mut input: impl BufRead) -> Result<Self, WlaSymbolsError> {
        let mut line = String::new();
        let mut current_section = Section::BeforeFirst;
        let mut wla_symbols = WlaSymbols::new();

        loop {
            line.clear();
            input.read_line(&mut line)?;
            if line.is_empty() { break; }

            let mut src: &str = &line;

            // Ignore comments
            if let Some(pos) = src.find(';') {
                src = &src[..pos];
            }

            // Ignore surrounding whitespace
            src = src.trim();

            // Skip blank lines
            if src.is_empty() { continue; }

            // Check if this line is the start of a new section
            if let Some(cap) = SECTION_REGEX.captures(src) {
                current_section = match &cap[1] {
                    "labels" => Section::Labels,
                    "symbols" => Section::Symbols,
                    "breakpoints" => Section::Breakpoints,
                    "definitions" => Section::Definitions,
                    "source files" => Section::SourceFiles,
                    "rom checksum" => Section::RomChecksum,
                    "addr-to-line mapping" => Section::AddrLineMappings,
                    section => {
                        warn!("ignoring unknown section [{}] in WLA DX symbol file", section);
                        Section::Unknown
                    }
                };
                continue;
            }

            let regex = match current_section {
                Section::Labels => &*LABEL_REGEX,
                Section::Definitions => &*DEFINITION_REGEX,
                Section::Breakpoints => &*BREAKPOINT_REGEX,
                Section::Symbols => &*SYMBOL_REGEX,
                Section::SourceFiles => &*SOURCE_FILE_REGEX,
                Section::RomChecksum => &*ROM_CHECKSUM_REGEX,
                Section::AddrLineMappings => &*ADDR_LINE_MAPPING_REGEX,

                Section::BeforeFirst => {
                    return Err(WlaSymbolsError::TextBeforeFirstSection(String::from(src)));
                }

                // Ignore lines in sections we don't know how to parse.
                Section::Unknown => continue,
            };

            let captures = match regex.captures(src) {
                Some(cap) => cap,
                None => return Err(WlaSymbolsError::NonmatchingLine {
                    actual: String::from(src),
                    expected_regex: String::from(regex.as_str()),
                }),
            };

            match current_section {
                Section::Labels => {
                    let bank = u8::from_str_radix(&captures[1], 16).unwrap();
                    let addr = u16::from_str_radix(&captures[2], 16).unwrap();
                    let label = String::from(&captures[3]);
                    wla_symbols.labels.insert(RomAddr { bank, addr }, label);
                }

                Section::Symbols => {
                    let bank = u8::from_str_radix(&captures[1], 16).unwrap();
                    let addr = u16::from_str_radix(&captures[2], 16).unwrap();
                    let label = String::from(&captures[3]);
                    wla_symbols.symbols.insert(label, RomAddr { bank, addr });
                }

                Section::Breakpoints => {
                    let bank = u8::from_str_radix(&captures[1], 16).unwrap();
                    let addr = u16::from_str_radix(&captures[2], 16).unwrap();
                    wla_symbols.breakpoints.push(RomAddr { bank, addr });
                }

                Section::Definitions => {
                    let value = usize::from_str_radix(&captures[1], 16).unwrap();
                    let name = String::from(&captures[2]);
                    wla_symbols.definitions.insert(name, value);
                }

                Section::SourceFiles => {
                    let index = usize::from_str_radix(&captures[1], 16).unwrap();
                    let crc32 = u32::from_str_radix(&captures[2], 16).unwrap();
                    let path = String::from(&captures[3]);
                    wla_symbols.source_files.insert(index, SourceFile { crc32, path });
                }

                Section::RomChecksum => {
                    if wla_symbols.rom_crc32.is_some() {
                        return Err(WlaSymbolsError::MultipleRomChecksums);
                    }

                    let crc32 = u32::from_str_radix(&captures[1], 16).unwrap();
                    wla_symbols.rom_crc32 = Some(crc32);
                }

                Section::AddrLineMappings => {
                    let bank = u8::from_str_radix(&captures[1], 16).unwrap();
                    let addr = u16::from_str_radix(&captures[2], 16).unwrap();
                    let source_file_index = usize::from_str_radix(&captures[3], 16).unwrap();
                    let line = usize::from_str_radix(&captures[4], 16).unwrap();
                    wla_symbols.addr_to_line.insert(
                        RomAddr { bank, addr },
                        SourceLine { source_file_index, line },
                    );
                }

                Section::BeforeFirst | Section::Unknown => unreachable!(),
            }
        }

        Ok(wla_symbols)
    }
}