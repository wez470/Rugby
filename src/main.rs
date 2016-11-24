mod reg_16;
use reg_16::Reg16;

struct Cpu {
    reg_af: Reg16,
    reg_bc: Reg16,
    reg_de: Reg16,
    reg_hl: Reg16,
    reg_sp: Reg16,
    reg_pc: Reg16
}

impl Cpu {
    fn new() -> Cpu {
        Cpu {
            reg_af: Reg16::new(),
            reg_bc: Reg16::new(),
            reg_de: Reg16::new(),
            reg_hl: Reg16::new(),
            reg_sp: Reg16::new(),
            reg_pc: Reg16::new()
        }
    }
}

fn main() {
    let mut cpu = Cpu::new();

    let mut test_reg = Reg16::new();
    test_reg.set(60000);
    println!("Value in test reg: {}", test_reg.get());
    test_reg.inc();
    println!("Value in test reg: {}", test_reg.get());
}
