#[macro_use]
extern crate bitflags;
extern crate clap;

use cpu::Cpu;
use std::fs::File;
use std::io::Read;
use clap::{Arg, App, AppSettings};
use std::process::exit;
use std::fmt::Display;

mod cartridge;
mod cpu;
mod reg_16;

fn main() {
    let matches = App::new("Rustboy")
        .setting(AppSettings::ArgRequiredElseHelp)
        .arg(Arg::with_name("ROM")
             .required(true)
             .help("The game rom"))
        .arg(Arg::with_name("INSTRUCTIONS")
             .required(true)
             .help("The number of instructions to execute"))
        .get_matches();

    let rom_file_name = matches.value_of("ROM").unwrap();
    let mut file = check_error(File::open(rom_file_name), "Couldn't open rom file");
    let mut file_buf = Vec::new();
    check_error(file.read_to_end(&mut file_buf), "Couldn't read rom");
    let rom = file_buf.into_boxed_slice();

    let instruction_count = matches.value_of("INSTRUCTIONS").unwrap().parse().unwrap();

    let mut cpu = Cpu::new(rom);
    cpu.step_n(instruction_count);
}

fn check_error<T, E: Display>(res: Result<T, E>, message: &'static str) -> T {
    match res {
        Ok(r) => r,
        Err(e) => {
            println!("{}: {}", message, e);
            exit(1);
        }
    }
}
