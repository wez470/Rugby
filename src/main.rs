use crate::cart::Cart;
use crate::cpu::Cpu;
use crate::frontend::start_frontend;
use crate::wla_symbols::WlaSymbols;
use failure::ResultExt;
use log::info;
use std::fs::File;
use std::io::{BufReader, Write};
use std::path::PathBuf;
use structopt::StructOpt;

mod cart;
mod cart_header;
mod cpu;
mod frontend;
mod gpu;
mod interrupts;
mod joypad;
mod reg_16;
mod timer;
mod wla_symbols;

#[derive(Debug, StructOpt)]
#[structopt(name = "Rustboy", about = "A Game Boy emulator")]
enum Opts {
    #[structopt(name = "run", about = "Runs the given Game Boy ROM file")]
    Run(RunOpts),

    #[structopt(name = "info", about = "Prints information about the given Game Boy ROM")]
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
    /// The game ROM file path
    #[structopt(name = "ROM", parse(from_os_str))]
    rom_path: PathBuf,
}

fn main() -> Result<(), failure::Error> {
    let env = env_logger::Env::new().filter("RUSTBOY_LOG").write_style("RUSTBOY_LOG_STYLE");
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
    let cart_header = cart_header::CartHeader::from_rom(&rom)
        .context("Failed to parse cartridge header")?;

    // TODO(solson): Include some kind of game-identifying information in the save file to
    // prevent loading a save file with the wrong game.
    let ram = opts.save_path
        .as_ref()
        .and_then(|path| std::fs::read(path).ok())
        .map(|r| r.into_boxed_slice());
    if ram.is_some() {
        info!("Initialized cartridge RAM from file");
    }

    let cart = Cart::new(rom, ram, &cart_header).context("Failed to initialize cartridge")?;
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
    let rom = std::fs::read(&opts.rom_path).context("Failed to read ROM file")?;
    let cart = cart_header::CartHeader::from_rom(&rom)
        .context("Couldn't parse cartridge header")?;

    let mut out = tabwriter::TabWriter::new(std::io::stdout());

    match std::str::from_utf8(&cart.title) {
        Ok(title) => writeln!(out, "Title:\t{}", title)?,
        Err(_) => writeln!(out, "Title:\t{:x?}", cart.title)?,
    }
    writeln!(out, "Version:\t{}", cart.rom_version)?;
    writeln!(out, "MBC type:\t{:?}", cart.cart_type.mbc)?;
    writeln!(out, "Hardware:\t{:?}", cart.cart_type.hardware)?;
    writeln!(out, "ROM size:\t{:?} KiB", cart.rom_size / 1024)?;
    writeln!(out, "RAM size:\t{:?} KiB", cart.ram_size / 1024)?;
    writeln!(out, "GBC support:\t{:?}", cart.gbc_flag)?;
    writeln!(out, "SGB support:\t{:?}", cart.sgb_flag)?;
    writeln!(out, "Manufacturer code:\t{:?}", cart.manufacturer_code)?;
    writeln!(out, "Licensee code:\t{:?}", cart.licensee_code)?;
    writeln!(out, "Destination code:\t{:?}", cart.destination_code)?;

    out.flush()?;
    Ok(())
}