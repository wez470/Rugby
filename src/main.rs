use crate::cart::{Cart, CartConfig};
use crate::cart_header::{CartHardware, CartHeader};
use crate::cpu::Cpu;
use crate::frontend::start_frontend;
use crate::wla_symbols::WlaSymbols;
use failure::ResultExt;
use log::info;
use std::fs::File;
use std::io::{BufReader, Write};
use std::path::PathBuf;
use structopt::StructOpt;

mod audio;
mod cart;
mod cart_header;
mod cpu;
mod frontend;
mod gpu;
mod interrupts;
mod joypad;
mod timer;
mod wla_symbols;

#[derive(Debug, StructOpt)]
#[structopt(name = "Rugby", about = "Rust Game Boy? Yes!")]
enum Opts {
    #[structopt(name = "run", about = "Runs the given Game Boy ROM file")]
    Run(RunOpts),

    #[structopt(name = "info", about = "Prints information about the given Game Boy ROMs")]
    Info(InfoOpts),
}

#[derive(Debug, StructOpt)]
struct RunOpts {
    /// The game ROM file path
    #[structopt(name = "ROM", parse(from_os_str))]
    rom_path: PathBuf,

    /// Load and save cartridge RAM to this file
    #[structopt(short = "s", long = "save-file", name = "SAVE", parse(from_os_str))]
    save_path: Option<PathBuf>,

    /// Load symbol file for debugging (in the WLA DX assembler's format
    #[structopt(short = "S", long = "symbol-file", name = "SYMBOLS", parse(from_os_str))]
    symbols_path: Option<PathBuf>,
}

#[derive(Debug, StructOpt)]
struct InfoOpts {
    /// The game ROM file paths
    #[structopt(name = "ROM", parse(from_os_str), required = true)]
    rom_path: Vec<PathBuf>,

    /// Show results in a table
    #[structopt(short = "t", long = "table")]
    table: bool,
}

fn main() -> Result<(), failure::Error> {
    let env = env_logger::Env::new().filter("RUGBY_LOG").write_style("RUGBY_LOG_STYLE");
    env_logger::Builder::from_env(env)
        .default_format_timestamp(false)
        .init();

    match &Opts::from_args() {
        Opts::Run(run_opts) => run(run_opts),
        Opts::Info(info_opts) => info(info_opts),
    }
}

fn run(opts: &RunOpts) -> Result<(), failure::Error> {
    let rom = std::fs::read(&opts.rom_path)
        .context("Failed to read ROM file")?
        .into_boxed_slice();
    let cart_header = CartHeader::from_rom(&rom).context("Failed to parse cartridge header")?;
    let cart_config = CartConfig::from_cart_header(&cart_header)?;

    // TODO(solson): Include some kind of game-identifying information in the save file to
    // prevent loading a save file with the wrong game.
    let ram = opts.save_path
        .as_ref()
        .and_then(|path| std::fs::read(path).ok())
        .map(|r| r.into_boxed_slice());
    if ram.is_some() {
        info!("Initialized cartridge RAM from file");
    }

    let cart = Cart::new(rom, ram, &cart_config).context("Failed to initialize cartridge")?;
    let mut cpu = Cpu::new(cart);

    if let Some(path) = &opts.symbols_path {
        let file = File::open(path).context("Failed to open symbol file")?;
        cpu.debug_symbols = Some(WlaSymbols::parse(BufReader::new(file))
            .context("Failed to parse WLA DX symbol file")?);
    }

    start_frontend(&mut cpu);

    if let Some(path) = &opts.save_path {
        std::fs::write(path, cpu.cart.ram()).context("Failed to write to save file")?;
        info!("Successfully wrote cartridge RAM to save file");
    }

    Ok(())
}

fn info(opts: &InfoOpts) -> Result<(), failure::Error> {
    if opts.table {
        info_table(opts)
    } else {
        info_records(opts)
    }
}

/// Print the ROM info in a table with one ROM per row
fn info_table(opts: &InfoOpts) -> Result<(), failure::Error> {
    let mut out = tabwriter::TabWriter::new(std::io::stdout());
    writeln!(out, "Title\tVersion\tType\tHardware\tROM size\tRAM size\tGBC\tSGB\tLicensee\tDestination\tManufacturer")?;

    for path in &opts.rom_path {
        let rom = std::fs::read(path)
            .with_context(|_| format!("Failed to read ROM file: {}", path.display()))?;
        let cart = cart_header::CartHeader::from_rom(&rom)
            .with_context(|_| format!("Failed to parse cartridge header: {}", path.display()))?;

        match std::str::from_utf8(&cart.title) {
            Ok(title) => write!(out, "{}\t", title)?,
            Err(_) => write!(out, "{:x?}\t", cart.title)?,
        }
        write!(out, "{}\t", cart.rom_version)?;
        write!(out, "{:?}\t", cart.cart_type)?;
        write!(out, "{}\t", CartHardware::flags_to_string(cart.hardware))?;
        write!(out, "{}\t", cart.rom_size)?;
        write!(out, "{}\t", cart.ram_size)?;
        write!(out, "{}\t", cart.gbc_flag)?;
        write!(out, "{}\t", cart.sgb_flag)?;
        write!(out, "{:?}\t", cart.licensee_code)?;
        write!(out, "{:?}\t", cart.destination_code)?;
        write!(out, "{:?}", cart.manufacturer_code)?;
        writeln!(out, "")?;
    }

    out.flush()?;
    Ok(())
}

/// Print the ROM info in the style of a list of separate key-value records.
fn info_records(opts: &InfoOpts) -> Result<(), failure::Error> {
    for path in &opts.rom_path {
        let rom = std::fs::read(path)
            .with_context(|_| format!("Failed to read ROM file: {}", path.display()))?;
        let cart = cart_header::CartHeader::from_rom(&rom)
            .with_context(|_| format!("Failed to parse cartridge header: {}", path.display()))?;
        let mut out = tabwriter::TabWriter::new(std::io::stdout());

        match std::str::from_utf8(&cart.title) {
            Ok(title) => writeln!(out, "Title:\t{}", title)?,
            Err(_) => writeln!(out, "Title:\t{:x?}", cart.title)?,
        }
        writeln!(out, "Version:\t{}", cart.rom_version)?;
        writeln!(out, "MBC type:\t{:?}", cart.cart_type)?;
        writeln!(out, "Hardware:\t{}", CartHardware::flags_to_string(cart.hardware))?;
        writeln!(out, "ROM size:\t{}", cart.rom_size)?;
        writeln!(out, "RAM size:\t{}", cart.ram_size)?;
        writeln!(out, "GBC support:\t{}", cart.gbc_flag)?;
        writeln!(out, "SGB support:\t{}", cart.sgb_flag)?;
        writeln!(out, "Manufacturer code:\t{:?}", cart.manufacturer_code)?;
        writeln!(out, "Licensee code:\t{:?}", cart.licensee_code)?;
        writeln!(out, "Destination code:\t{:?}", cart.destination_code)?;
        writeln!(out, "")?;

        out.flush()?;
    }
    Ok(())
}
