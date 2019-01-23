use clap;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use crate::cpu::Cpu;
use crate::cart::Cart;
use crate::frontend::start_frontend;

mod cart;
mod cart_header;
mod cpu;
mod frontend;
mod gpu;
mod interrupts;
mod reg_16;
mod timer;

fn main() {
    let app_matches = clap::App::new("Rustboy")
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        .subcommand(clap::SubCommand::with_name("run")
            .arg(clap::Arg::with_name("ROM")
                .required(true)
                .help("The game rom"))
            .arg(clap::Arg::with_name("INSTRUCTIONS")
                .required(true)
                .help("The number of instructions to execute"))
            .arg(clap::Arg::with_name("Step Mode")
                .short("s")
                .help("Allows step mode where 'space' will execute one frame")))
        .subcommand(clap::SubCommand::with_name("info")
            .arg(clap::Arg::with_name("ROM")
                .required(true)
                .help("The game rom")))
        .get_matches();

    match app_matches.subcommand() {
        ("run", Some(matches)) => {
            let rom_path = matches.value_of("ROM").unwrap();
            let rom = read_rom_file(rom_path);
            let instruction_count: usize = matches.value_of("INSTRUCTIONS").unwrap().parse()
                .expect("Couldn't parse instruction count");
            let cart_header = cart_header::CartHeader::from_rom(&rom)
                .expect("Couldn't parse cartridge header");
            let cart = Cart::new(rom, &cart_header);
            let mut cpu = Cpu::new(cart);

            start_frontend(&mut cpu, instruction_count, matches.is_present("Step Mode"));
        }


        ("info", Some(matches)) => {
            let rom_path = matches.value_of("ROM").unwrap();
            let rom = read_rom_file(rom_path);
            let cart_header = cart_header::CartHeader::from_rom(&rom)
                .expect("Couldn't parse cartridge header");
            println!("{:#?}", cart_header);
        }

        _ => unreachable!(),
    }
}

fn read_rom_file<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut file = File::open(path).expect("Couldn't open rom file");
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).expect("Couldn't read rom");
    file_buf.into_boxed_slice()
}
