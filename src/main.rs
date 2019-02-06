use crate::cart::Cart;
use crate::cpu::Cpu;
use crate::frontend::start_frontend;
use log::info;

mod cart;
mod cart_header;
mod cpu;
mod frontend;
mod gpu;
mod interrupts;
mod joypad;
mod reg_16;
mod timer;

fn main() {
    let env = env_logger::Env::new().filter("RUSTBOY_LOG").write_style("RUSTBOY_LOG_STYLE");
    env_logger::init_from_env(env);

    let app_matches = clap::App::new("Rustboy")
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        .subcommand(clap::SubCommand::with_name("run")
            .arg(clap::Arg::with_name("ROM")
                .required(true)
                .help("The game rom"))
            .arg(clap::Arg::with_name("SAVE")
                .short("s")
                .long("save-file")
                .takes_value(true)
                .help("Load and save cartridge RAM to this file")))
        .subcommand(clap::SubCommand::with_name("info")
            .arg(clap::Arg::with_name("ROM")
                .required(true)
                .help("The game rom")))
        .get_matches();

    match app_matches.subcommand() {
        ("run", Some(matches)) => {
            let rom_path = matches.value_of("ROM").unwrap();
            let rom = std::fs::read(rom_path).expect("Failed to read ROM file").into_boxed_slice();
            let cart_header = cart_header::CartHeader::from_rom(&rom)
                .expect("Failed to parse cartridge header");

            // TODO(solson): Include some kind of game-identifying information in the save file to
            // prevent loading a save file with the wrong game.
            let save_path_opt = matches.value_of_os("SAVE");
            let ram = save_path_opt
                .and_then(|path| std::fs::read(path).ok())
                .map(|r| r.into_boxed_slice());
            if ram.is_some() {
                info!("Initialized cartridge RAM from file");
            }

            let cart = Cart::new(rom, ram, &cart_header).expect("Failed to initialize cartridge");
            let mut cpu = Cpu::new(cart);
            start_frontend(&mut cpu);

            if let Some(path) = save_path_opt {
                let res = std::fs::write(path, cpu.cart.ram());
                info!("Wrote cartridge RAM to file: {:?}", res);
            }
        }


        ("info", Some(matches)) => {
            let rom_path = matches.value_of("ROM").unwrap();
            let rom = std::fs::read(rom_path).expect("Failed to read ROM file").into_boxed_slice();
            let cart_header = cart_header::CartHeader::from_rom(&rom)
                .expect("Couldn't parse cartridge header");
            println!("{:#?}", cart_header);
        }

        _ => unreachable!(),
    }
}