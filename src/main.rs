extern crate clap;

use memory::Memory;
use cpu::Cpu;
use std::env;
use std::fs::File;
use std::io::Read;
use clap::{Arg, App, AppSettings};
use std::process::exit;
use std::fmt::Display;

mod memory;
mod cpu;
mod reg_16;

fn main() {
    let matches = App::new("Rustboy")
        .setting(AppSettings::ArgRequiredElseHelp)
        .arg(Arg::with_name("ROM")
             .required(true)
             .help("The game rom"))
        .get_matches();

    let rom_file_name = matches.value_of("ROM").unwrap();
    let mut file = check_error(File::open(rom_file_name), "Couldn't open rom file");
    let mut file_buf = Vec::new();
    check_error(file.read_to_end(&mut file_buf), "Couldn't read rom");
    let rom = file_buf.into_boxed_slice();

    let mut mem = Memory::new();
    let mut cpu = Cpu::new(rom, mem);
    cpu.reset();
    for _ in 0..10 {
        cpu.run();
    }
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
