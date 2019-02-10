use crate::cart::Cart;
use crate::cpu::Cpu;
use crate::frontend::start_frontend;
use crate::wla_symbols::WlaSymbols;
use log::info;
use std::fs::File;
use std::io::BufReader;
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

fn main() {
    let env = env_logger::Env::new().filter("RUSTBOY_LOG").write_style("RUSTBOY_LOG_STYLE");
    env_logger::Builder::from_env(env)
        .default_format_timestamp(false)
        .init();

    let opts = Opts::from_args();

    match &opts {
        Opts::Run(run_opts) => {
            let rom = std::fs::read(&run_opts.rom_path)
                .expect("Failed to read ROM file")
                .into_boxed_slice();
            let cart_header = cart_header::CartHeader::from_rom(&rom)
                .expect("Failed to parse cartridge header");

            // TODO(solson): Include some kind of game-identifying information in the save file to
            // prevent loading a save file with the wrong game.
            let ram = run_opts.save_path
                .as_ref()
                .and_then(|path| std::fs::read(path).ok())
                .map(|r| r.into_boxed_slice());
            if ram.is_some() {
                info!("Initialized cartridge RAM from file");
            }

            let symbols = run_opts.symbols_path.as_ref().map(|path| {
                let file = File::open(path).expect("Failed to open symbol file");
                WlaSymbols::parse(BufReader::new(file))
                    .expect("Failed to parse WLA DX symbol file")
            });

            let cart = Cart::new(rom, ram, &cart_header).expect("Failed to initialize cartridge");
            let mut cpu = Cpu::new(cart);
            cpu.debug_symbols = symbols;
            start_frontend(&mut cpu);

            if let Some(path) = &run_opts.symbols_path {
                let res = std::fs::write(path, cpu.cart.ram());
                info!("Wrote cartridge RAM to file: {:?}", res);
            }
        }

        Opts::Info(info_opts) => {
            let rom = std::fs::read(&info_opts.rom_path).expect("Failed to read ROM file");
            let cart = cart_header::CartHeader::from_rom(&rom)
                .expect("Couldn't parse cartridge header");

            let mut out = tabwriter::TabWriter::new(std::io::stdout());

            use std::io::Write;
            match std::str::from_utf8(&cart.title) {
                Ok(title) => { writeln!(out, "Title:\t{}", title).unwrap(); }
                Err(_) => { writeln!(out, "Title:\t{:x?}", cart.title).unwrap(); }
            }
            writeln!(out, "Version:\t{}", cart.rom_version).unwrap();
            writeln!(out, "MBC type:\t{:?}", cart.cart_type.mbc).unwrap();
            writeln!(out, "Hardware:\t{:?}", cart.cart_type.hardware).unwrap();
            writeln!(out, "ROM size:\t{:?} KiB", cart.rom_size / 1024).unwrap();
            writeln!(out, "RAM size:\t{:?} KiB", cart.ram_size / 1024).unwrap();
            writeln!(out, "GBC support:\t{:?}", cart.gbc_flag).unwrap();
            writeln!(out, "SGB support:\t{:?}", cart.sgb_flag).unwrap();
            writeln!(out, "Manufacturer code:\t{:?}", cart.manufacturer_code).unwrap();
            writeln!(out, "Licensee code:\t{:?}", cart.licensee_code).unwrap();
            writeln!(out, "Destination code:\t{:?}", cart.destination_code).unwrap();

            out.flush().unwrap();
        }
    }
}