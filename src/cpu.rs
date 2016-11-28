use reg_16::Reg16;
use memory::Memory;

pub struct Cpu {
    reg_af: Reg16,
    reg_bc: Reg16,
    reg_de: Reg16,
    reg_hl: Reg16,
    reg_sp: Reg16,
    reg_pc: Reg16,
    memory: Memory,
    rom: Box<[u8]>,
}

impl Cpu {
    pub fn new(rom: Box<[u8]>, mem: Memory) -> Cpu {
        let mut cpu = Cpu {
            reg_af: Reg16::new(),
            reg_bc: Reg16::new(),
            reg_de: Reg16::new(),
            reg_hl: Reg16::new(),
            reg_sp: Reg16::new(),
            reg_pc: Reg16::new(),
            memory: mem,
            rom: rom,
        };
        cpu.reset();
        cpu
    }

    pub fn reset(&mut self) {
        self.reg_pc.set(0x0100);
        self.reg_af.high = 0x01;
        self.reg_af.low = 0xB0;
        self.reg_bc.set(0x0013);
        self.reg_de.set(0x00D8);
        self.reg_hl.set(0x014D);
        self.reg_sp.set(0xFFFE);
    }

    pub fn run(&mut self) {
        let instruction = self.rom[self.reg_pc.get() as usize];
        println!("{:02X}", instruction);
        self.reg_pc.inc();
        //fetchOpcode
        //executeOpcode
        //update timers with num cycles
        //update graphics
        //Interrupts
    }


}
