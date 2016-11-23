struct CPU {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16
}

impl CPU {
    fn new() -> CPU {
        CPU {
            af: 0,
            bc: 0,
            de: 0,
            hl: 0,
            sp: 0,
            pc: 0
        }
    }
}

fn main() {
    println!("Hello, world!");

    let mut cpu = CPU::new();
}
