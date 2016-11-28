mod memory;
mod cpu;
mod reg_16;

use memory::Memory;
use cpu::Cpu;
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let rom_file_name = env::args().nth(1).unwrap();
    let mut file = File::open(rom_file_name).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();
    let rom = file_buf.into_boxed_slice();

    let mut mem = Memory::new();
    let mut cpu = Cpu::new(rom, &mut mem);
    cpu.reset();
    for _ in 0..10 {
        cpu.run();
    }
}
