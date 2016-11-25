use reg_16::Reg16;

pub struct Cpu {
    reg_af: Reg16,
    reg_bc: Reg16,
    reg_de: Reg16,
    reg_hl: Reg16,
    reg_sp: Reg16,
    reg_pc: Reg16
}

impl Cpu {
    pub fn new() -> Cpu {
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
