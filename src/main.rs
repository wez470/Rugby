mod cpu;
mod reg_16;

use cpu::Cpu;
use reg_16::Reg16;

fn main() {
    let mut cpu = Cpu::new();

    let mut test_reg = Reg16::new();
    test_reg.set(60000);
    println!("Value in test reg: {}", test_reg.get());
    test_reg.inc();
    println!("Value in test reg: {}", test_reg.get());
}
