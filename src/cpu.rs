use reg_16::Reg16;
use memory::Memory;

pub const BASE_CYCLES: [usize; 0x100] = [
//   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
     4, 12,  8,  8,  4,  4,  8,  4, 20,  8,  8,  8,  4,  4,  8,  4, // 0
     4, 12,  8,  8,  4,  4,  8,  4, 12,  8,  8,  8,  4,  4,  8,  4, // 1
     8, 12,  8,  8,  4,  4,  8,  4,  8,  8,  8,  8,  4,  4,  8,  4, // 2
     8, 12,  8,  8, 12, 12, 12,  4,  8,  8,  8,  8,  4,  4,  8,  4, // 3
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 4
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 5
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 6
     8,  8,  8,  8,  8,  8,  4,  8,  4,  4,  4,  4,  4,  4,  8,  4, // 7
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 8
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // 9
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // A
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4, // B
     8, 12, 12, 16, 12, 16,  8, 16,  8, 16, 12,  0, 12, 24,  8, 16, // C
     8, 12, 12,  0, 12, 16,  8, 16,  8, 16, 12,  0, 12,  0,  8, 16, // D
    12, 12,  8,  0,  0, 16,  8, 16, 16,  4, 16,  0,  0,  0,  8, 16, // E
    12, 12,  8,  4,  0, 16,  8, 16, 12,  8, 16,  4,  0,  0,  8, 16, // F
];

pub const INSTRUCTION_LENGTH: [usize; 0x100] = [
//   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
     1,  3,  1,  1,  1,  1,  2,  1,  3,  1,  1,  1,  1,  1,  2,  1, // 0
     2,  3,  1,  1,  1,  1,  2,  1,  2,  1,  1,  1,  1,  1,  2,  1, // 1
     2,  3,  1,  1,  1,  1,  2,  1,  2,  1,  1,  1,  1,  1,  2,  1, // 2
     2,  3,  1,  1,  1,  1,  2,  1,  2,  1,  1,  1,  1,  1,  2,  1, // 3
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 4
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 5
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 6
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 7
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 8
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // 9
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, // A
     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1, // B
     1,  1,  3,  3,  3,  1,  2,  1,  1,  1,  3,  1,  3,  3,  2,  1, // C
     1,  1,  3,  0,  3,  1,  2,  1,  1,  1,  3,  0,  3,  0,  2,  1, // D
     2,  1,  2,  0,  0,  1,  2,  1,  2,  1,  3,  0,  0,  0,  2,  1, // E
     2,  1,  2,  1,  0,  1,  2,  1,  2,  1,  3,  1,  0,  0,  2,  1, // F
];

pub const PREFIX_CB_BASE_CYCLES: [usize; 0x100] = [
//   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  A,  B,  C,  D,  E,  F
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 0
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 1
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 2
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 3
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 4
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 5
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 6
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 7
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 8
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // 9
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // A
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // B
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // C
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // D
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // E
     8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8, // F
];

// Every CB-prefixed instruction has length 2 (counting the CB opcode).
pub const PREFIX_CB_INSTRUCTION_LENGTH: usize = 2;

fn base_cycles(opcode: u8) -> usize {
    if opcode == 0xCB {
        PREFIX_CB_BASE_CYCLES[opcode as usize]
    } else {
        BASE_CYCLES[opcode as usize]
    }
}

fn instruction_length(opcode: u8) -> usize {
    if opcode == 0xCB {
        PREFIX_CB_INSTRUCTION_LENGTH
    } else {
        INSTRUCTION_LENGTH[opcode as usize]
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Regs_8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Clone, Copy, Debug)]
pub enum Regs_16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

/// Represents an operand resolving to an 8-bit value.
#[derive(Debug)]
enum Operand8 {
    /// 8-bit immediate value.
    Imm8(u8),

    /// 8-bit register.
    Reg8(Regs_8),

    /// Memory location at the given immediate address.
    MemImm(u16),

    /// Memory location at the address `0xFF00 + byte`.
    MemImmHigh(u8),

    /// Memory location at the address in the given register.
    MemReg(Regs_16),
}

/// Represents an operand resolving to a 16-bit value.
#[derive(Debug)]
enum Operand16 {
    /// 16-bit immediate value.
    Imm16(u16),

    /// 16-bit register.
    Reg16(Regs_16),
}


/// Represents the condition checked by a conditional instruction (JP, JR, RET, or CALL).
#[derive(Debug)]
enum Cond {
    /// Unconditional.
    None,

    /// Z: zero flag is set.
    Zero,

    /// NZ: zero flag is unset.
    NotZero,

    /// C: carry flag is set.
    Carry,

    /// NC: carry flag is unset.
    NotCarry,
}

#[derive(Debug)]
enum Inst {
    /// NOP: No operation.
    Nop,

    /// STOP: Halt CPU & LCD display until button pressed.
    Stop,

    /// HALT: Power down CPU until an interrupt occurs. The Gameboy uses this to save power.
    Halt,

    /// DI: Disable interrupts.
    Di,

    /// EI: Enable interrupts.
    Ei,

    /// JP: Absolute jump.
    Jp(u16, Cond),

    /// JR: Relative jump.
    Jr(i8, Cond),

    /// CALL: Call the function at the given immediate address.
    Call(u16, Cond),

    /// RET: Return from the current function by jumping to an address popped from the stack.
    Ret,

    /// LD: Loads, stores, and moves for 8 bit values
    Ld8(Operand8, Operand8),

    /// LD: Loads, stores, and moves for 16 bit values
    Ld16(Operand16, Operand16),

    /// XOR: Exclusive-or between A and the operand.
    Xor(Operand8),

    /// CP: Compare A with the operand. Like `A - operand` but only for the flag side effects.
    Cp(Operand8),

    /// RLC: Rotate left 1 bit. Does not flow through carry, despite the name. :)
    Rlc(Operand8),

    /// RL: Rotate left 1 bit. This one does flow through carry, despite the name. :)
    Rl(Operand8),

    /// RRC: Rotate right 1 bit. Does not flow through carry, despite the name. :)
    Rrc(Operand8),

    /// RR: Rotate right 1 bit. This one does flow through carry, despite the name. :)
    Rr(Operand8),

    /// BIT: Test the bit at the given index in the given operand. Sets the zero flag accordingly.
    Bit(u8, Operand8),

    /// RES: Reset the bit at the given index in the given operand. (Set to zero.)
    Res(u8, Operand8),

    /// SET: Set the bit at the given index in the given operand. (Set to one.)
    Set(u8, Operand8),
}

#[derive(Clone)]
pub struct Cpu {
    /// Register containing the flags register and register 'A'
    reg_af: Reg16,

    /// Register containing registers 'B' and 'C'
    reg_bc: Reg16,

    /// Register containing registers 'D' and 'E'
    reg_de: Reg16,

    /// Register containing registers 'H' and 'L'
    reg_hl: Reg16,

    /// Register that contains the stack pointer
    reg_sp: Reg16,

    /// Register that contains the program counter
    reg_pc: Reg16,

    /// RAM
    memory: Memory,

    /// ROM (game cartridge)
    rom: Box<[u8]>,

    /// The location of the start of the currently-executing instruction.
    base_pc: usize,

    /// A running total of the number of cycles taken in execution so far.
    cycles: usize,

    /// True if interrupts can currently execute.
    interrupts_enabled: bool,

    /// True if the previous instruction was DI, which disables interrupts after the next
    /// instruction.
    pending_disable_interrupts: bool,

    /// True if the previous instruction was EI, which enables interrupts after the next
    /// instruction.
    pending_enable_interrupts: bool,
}

impl Cpu {
    pub fn new(rom: Box<[u8]>, mem: Memory) -> Cpu {
        let mut cpu = Cpu {
            reg_af: Reg16::default(),
            reg_bc: Reg16::default(),
            reg_de: Reg16::default(),
            reg_hl: Reg16::default(),
            reg_sp: Reg16::default(),
            reg_pc: Reg16::default(),
            memory: mem,
            rom: rom,
            base_pc: 0,
            cycles: 0,
            interrupts_enabled: false,
            pending_disable_interrupts: false,
            pending_enable_interrupts: false,
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

    pub fn step(&mut self) {
        let pending_enable_interrupts = self.pending_enable_interrupts;
        let pending_disable_interrupts = self.pending_disable_interrupts;
        self.pending_enable_interrupts = false;
        self.pending_disable_interrupts = false;

        self.base_pc = self.reg_pc.get() as usize;
        let opcode = self.rom[self.base_pc];
        let instruction_len = instruction_length(opcode);
        self.cycles += base_cycles(opcode);

        print!("{:04X}:", self.base_pc);
        for &byte in &self.rom[self.base_pc .. (self.base_pc + instruction_len)] {
            print!(" {:02X}", byte);
        }

        self.reg_pc.inc(instruction_len as i8);

        // TODO(solson): We're in the middle of incrementally porting from the old `match opcode`
        // below to the new `self.execute(inst)` method. While this is happening we're tracking
        // which instructions are handled by each part with the `handled_*` bools.
        //
        // These should be removed when the porting is done.
        let handled_by_execute;
        if let Some(inst) = decode(&self.rom[self.base_pc..(self.base_pc + instruction_len)]) {
            println!("\t\t(decoded: {:?})", inst);
            handled_by_execute = self.execute(inst);
        } else {
            println!("\t\t(could not decode)");
            handled_by_execute = false;
        }

        let mut handled_by_opcode_match = true;
        match opcode {
            0x18 => self.jr_r8(),
            0x28 => self.jr_z_signed_8(),
            0xA8 => self.xor(Regs_8::B),
            0xA9 => self.xor(Regs_8::C),
            0xAA => self.xor(Regs_8::D),
            0xAB => self.xor(Regs_8::E),
            0xAC => self.xor(Regs_8::H),
            0xAD => self.xor(Regs_8::L),
            0xAF => self.xor(Regs_8::A),
            0xC3 => self.load_imm16(Regs_16::PC), // Note: this is a jump.
            0xCD => self.call(),
            0xC9 => self.ret(),
            0xFE => self.cp(),
            0xCB => {
                let opcode_after_cb = self.rom[self.base_pc + 1];
                match opcode_after_cb {
                    0x00 => self.rotate_left_carry(Regs_8::B),
                    0x01 => self.rotate_left_carry(Regs_8::C),
                    0x02 => self.rotate_left_carry(Regs_8::D),
                    0x03 => self.rotate_left_carry(Regs_8::E),
                    0x04 => self.rotate_left_carry(Regs_8::H),
                    0x05 => self.rotate_left_carry(Regs_8::L),
                    0x07 => self.rotate_left_carry(Regs_8::A),
                    0x08 => self.rotate_right_carry(Regs_8::B),
                    0x09 => self.rotate_right_carry(Regs_8::C),
                    0x0A => self.rotate_right_carry(Regs_8::D),
                    0x0B => self.rotate_right_carry(Regs_8::E),
                    0x0C => self.rotate_right_carry(Regs_8::H),
                    0x0D => self.rotate_right_carry(Regs_8::L),
                    0x0F => self.rotate_right_carry(Regs_8::A),
                    _ => handled_by_opcode_match = false,
                }
            }
            _ => handled_by_opcode_match = false,
        };

        match (handled_by_execute, handled_by_opcode_match) {
            (true, true) => panic!("instruction was handled twice"),
            (true, false) => {}, // Good case: only handled by new code.
            (false, true) => println!("  handled by old match"),
            (false, false) => println!("  unimplemented"),
        }

        if pending_enable_interrupts {
            self.interrupts_enabled = true;
        }

        if pending_disable_interrupts {
            self.interrupts_enabled = false;
        }
    }

    pub fn step_n(&mut self, steps: usize) {
        for _ in 0..steps {
            self.step();
        }
    }

    // TODO(solson): We should remove the "was handled" return value once all instructions are
    // handled.
    /// Execute the given instruction. Returns true if the instruction was handled.
    fn execute(&mut self, inst: Inst) -> bool {
        match inst {
            Inst::Nop => {},
            Inst::Di => self.pending_disable_interrupts = true,
            Inst::Ei => self.pending_enable_interrupts = true,
            Inst::Ld8(dest, src) => self.ld_8(dest, src),
            Inst::Ld16(dest, src) => self.ld_16(dest, src),
            _ => return false,
        }

        true
    }

    /// Load 8 bits from `src` and store them into `dest`.
    fn ld_8(&mut self, dest: Operand8, src: Operand8) {
        let val = self.get_operand_8(src);
        self.set_operand_8(dest, val);
    }

    fn get_operand_8(&self, src: Operand8) -> u8 {
        match src {
            Operand8::Imm8(val) => val,
            Operand8::Reg8(reg) => self.get_reg_8(reg),
            Operand8::MemImm(mem_loc) => self.memory.mem[mem_loc as usize],
            Operand8::MemImmHigh(mem_offset) => self.memory.mem[0xFF00 + mem_offset as usize],
            Operand8::MemReg(reg) => self.memory.mem[self.get_reg_16(reg) as usize],
        }
    }

    fn set_operand_8(&mut self, dest: Operand8, val: u8) {
        match dest {
            Operand8::Imm8(_) => panic!("Attempt to store to an 8-bit immediate value"),
            Operand8::Reg8(reg) => self.set_reg_8(reg, val),
            Operand8::MemImm(mem_loc) => self.memory.mem[mem_loc as usize] = val,
            Operand8::MemImmHigh(mem_offset) => self.memory.mem[0xFF00 + mem_offset as usize] = val,
            Operand8::MemReg(reg) => self.memory.mem[self.get_reg_16(reg) as usize] = val,
        }
    }

    /// Load 16 bits from `src` and store them into `dest`.
    fn ld_16(&mut self, dest: Operand16, src: Operand16) {
        let val = self.get_operand_16(src);
        self.set_operand_16(dest, val);
    }

    fn get_operand_16(&self, src: Operand16) -> u16 {
        match src {
            Operand16::Imm16(val) => val,
            Operand16::Reg16(reg) => self.get_reg_16(reg),
        }
    }

    fn set_operand_16(&mut self, dest: Operand16, val: u16) {
        match dest {
            Operand16::Imm16(_) => panic!("Attempt to store to a 16-bit immediate value"),
            Operand16::Reg16(reg) => self.set_reg_16(reg, val),
        }
    }

    /// Load immediate 16-bit data into the given register.
    fn load_imm16(&mut self, reg: Regs_16) {
        let low = self.rom[self.base_pc + 1] as u16;
        let high = self.rom[self.base_pc + 2] as u16;
        self.set_reg_16(reg, (high << 8) | low);
    }

    fn jr_r8(&mut self) {
        let n = self.rom[self.base_pc + 1] as i8;
        self.reg_pc.inc(n);
    }

    fn jr_z_signed_8(&mut self) {
        if self.get_zero_flag() {
            self.cycles += 4;
            let val = self.rom[self.base_pc + 1] as i8;
            self.reg_pc.inc(val);
        }
    }

    fn xor(&mut self, reg: Regs_8) {
        let n = self.get_reg_8(reg);
        let result = self.reg_af.high ^ n;
        self.set_zero_flag(result == 0);
        self.set_sub_flag(false);
        self.set_half_carry_flag(false);
        self.set_carry_flag(false);
    }

    fn cp(&mut self) {
        let a = self.reg_af.high;
        let n = self.rom[self.base_pc + 1];
        self.set_zero_flag(a == n);
        self.set_sub_flag(true);
        self.set_carry_flag(a < n);
        self.set_half_carry_flag(Cpu::get_sub_half_carry(a, n));
    }

    fn call(&mut self) {
        // The return address is the address of the instruction after the call.
        let return_addr = self.reg_pc.get();
        self.push_stack(return_addr);
        self.load_imm16(Regs_16::PC);
    }

    fn ret(&mut self) {
        let return_addr = self.pop_stack();
        self.set_reg_16(Regs_16::PC, return_addr);
    }

    fn push_stack(&mut self, val: u16) {
        self.reg_sp.inc(-2);
        let addr = self.reg_sp.get() as usize;
        self.memory.mem[addr] = val as u8; // low
        self.memory.mem[addr + 1] = (val >> 8) as u8; // high
    }

    fn pop_stack(&mut self) -> u16 {
        let addr = self.reg_sp.get() as usize;
        self.reg_sp.inc(2);
        let low = self.memory.mem[addr];
        let high = self.memory.mem[addr + 1];
        to_u16(low, high)
    }

    fn get_add_half_carry(first: u8, second: u8) -> bool {
       ((first & 0xf) + (second & 0xf)) & 0x10 == 0x10
    }

    fn get_sub_half_carry(first: u8, second: u8) -> bool {
       (first & 0xf) < (second & 0xf)
    }

    fn set_zero_flag(&mut self, set: bool) {
        self.reg_af.set_bit_8(set);
    }

    fn set_sub_flag(&mut self, set: bool) {
        self.reg_af.set_bit_7(set);
    }

    fn set_half_carry_flag(&mut self, set: bool) {
        self.reg_af.set_bit_6(set);
    }

    fn set_carry_flag(&mut self, set: bool) {
        self.reg_af.set_bit_5(set);
    }

    fn get_zero_flag(&self) -> bool {
        self.reg_af.is_bit_8_set()
    }

    fn set_reg_8(&mut self, reg: Regs_8, val: u8) {
        match reg {
            Regs_8::A => self.reg_af.high = val,
            Regs_8::F => self.reg_af.low = val,
            Regs_8::B => self.reg_bc.high = val,
            Regs_8::C => self.reg_bc.low = val,
            Regs_8::D => self.reg_de.high = val,
            Regs_8::E => self.reg_de.low = val,
            Regs_8::H => self.reg_hl.high = val,
            Regs_8::L => self.reg_hl.low = val,
        }
    }

    fn get_reg_8(&self, reg: Regs_8) -> u8 {
         match reg {
            Regs_8::A => self.reg_af.high,
            Regs_8::F => self.reg_af.low,
            Regs_8::B => self.reg_bc.high,
            Regs_8::C => self.reg_bc.low,
            Regs_8::D => self.reg_de.high,
            Regs_8::E => self.reg_de.low,
            Regs_8::H => self.reg_hl.high,
            Regs_8::L => self.reg_hl.low,
        }
    }

    fn set_reg_16(&mut self, reg: Regs_16, val: u16) {
        match reg {
            Regs_16::AF => self.reg_af.set(val),
            Regs_16::BC => self.reg_bc.set(val),
            Regs_16::DE => self.reg_de.set(val),
            Regs_16::HL => self.reg_hl.set(val),
            Regs_16::SP => self.reg_sp.set(val),
            Regs_16::PC => self.reg_pc.set(val),
        }
    }

    fn get_reg_16(&self, reg: Regs_16) -> u16 {
         match reg {
            Regs_16::AF => self.reg_af.get(),
            Regs_16::BC => self.reg_bc.get(),
            Regs_16::DE => self.reg_de.get(),
            Regs_16::HL => self.reg_hl.get(),
            Regs_16::SP => self.reg_sp.get(),
            Regs_16::PC => self.reg_pc.get(),
        }
    }

    /// Rotate register by one bit. The carry is the highest order bit value
    /// before the operation occurs. Rotation does not flow through carry bit
    fn rotate_left_carry(&mut self, reg: Regs_8) {
        let reg_val = self.get_reg_8(reg);
        let rot_val = reg_val.rotate_left(1);
        self.set_reg_8(reg, rot_val);
        self.set_zero_flag(rot_val == 0);
        self.set_half_carry_flag(false);
        self.set_sub_flag(false);
        self.set_carry_flag(reg_val & 0x80 != 0);
    }

    /// Rotate register by one bit. The carry is the lowest order bit value
    /// before the operation occurs. Rotation does not flow through carry bit
    fn rotate_right_carry(&mut self, reg: Regs_8) {
        let reg_val = self.get_reg_8(reg);
        let rot_val = reg_val.rotate_right(1);
        self.set_reg_8(reg, rot_val);
        self.set_zero_flag(rot_val == 0);
        self.set_half_carry_flag(false);
        self.set_sub_flag(false);
        self.set_carry_flag(reg_val & 0x01 != 0);
    }
}

/// Decode a Gameboy instruction from the given bytes.
///
/// Panics if the slice is empty or if it isn't long enough for the instruction specified by its
/// first byte (the opcode).
fn decode(bytes: &[u8]) -> Option<Inst> {
    use self::Inst::*;
    use self::Operand8::*;
    use self::Operand16::*;
    use self::Regs_16::*;
    use self::Regs_8::*;

    let inst = match bytes[0] {
        0x00 => Nop,
        0x06 => Ld8(Reg8(B), Imm8(bytes[1])),
        0x10 => {
            // FIXME: For some reason the STOP instruction is followed by 0x00 according to the
            // manual. Perhaps this should result in an invalid instruction if it's not zero. For
            // now we'll assert, so it's obvious if we ever encounter this case.
            assert_eq!(bytes[1], 0);
            Stop
        }
        0x11 => Ld16(Reg16(DE), Imm16(to_u16(bytes[1], bytes[2]))),
        0x18 => Jr(bytes[1] as i8, Cond::None),
        0x20 => Jr(bytes[1] as i8, Cond::NotZero),
        0x28 => Jr(bytes[1] as i8, Cond::Zero),
        0x30 => Jr(bytes[1] as i8, Cond::NotCarry),
        0x38 => Jr(bytes[1] as i8, Cond::Carry),
        0x3E => Ld8(Reg8(A), Imm8(bytes[1])),
        0x40 => Ld8(Reg8(B), Reg8(B)),
        0x41 => Ld8(Reg8(B), Reg8(C)),
        0x42 => Ld8(Reg8(B), Reg8(D)),
        0x43 => Ld8(Reg8(B), Reg8(E)),
        0x44 => Ld8(Reg8(B), Reg8(H)),
        0x45 => Ld8(Reg8(B), Reg8(L)),
        0x46 => Ld8(Reg8(B), MemReg(HL)),
        0x47 => Ld8(Reg8(B), Reg8(A)),
        0x48 => Ld8(Reg8(C), Reg8(B)),
        0x49 => Ld8(Reg8(C), Reg8(C)),
        0x4A => Ld8(Reg8(C), Reg8(D)),
        0x4B => Ld8(Reg8(C), Reg8(E)),
        0x4C => Ld8(Reg8(C), Reg8(H)),
        0x4D => Ld8(Reg8(C), Reg8(L)),
        0x4E => Ld8(Reg8(C), MemReg(HL)),
        0x4F => Ld8(Reg8(C), Reg8(A)),
        0x50 => Ld8(Reg8(D), Reg8(B)),
        0x51 => Ld8(Reg8(D), Reg8(C)),
        0x52 => Ld8(Reg8(D), Reg8(D)),
        0x53 => Ld8(Reg8(D), Reg8(E)),
        0x54 => Ld8(Reg8(D), Reg8(H)),
        0x55 => Ld8(Reg8(D), Reg8(L)),
        0x56 => Ld8(Reg8(D), MemReg(HL)),
        0x57 => Ld8(Reg8(D), Reg8(A)),
        0x58 => Ld8(Reg8(E), Reg8(B)),
        0x59 => Ld8(Reg8(E), Reg8(C)),
        0x5A => Ld8(Reg8(E), Reg8(D)),
        0x5B => Ld8(Reg8(E), Reg8(E)),
        0x5C => Ld8(Reg8(E), Reg8(H)),
        0x5D => Ld8(Reg8(E), Reg8(L)),
        0x5E => Ld8(Reg8(E), MemReg(HL)),
        0x5F => Ld8(Reg8(E), Reg8(A)),
        0x60 => Ld8(Reg8(H), Reg8(B)),
        0x61 => Ld8(Reg8(H), Reg8(C)),
        0x62 => Ld8(Reg8(H), Reg8(D)),
        0x63 => Ld8(Reg8(H), Reg8(E)),
        0x64 => Ld8(Reg8(H), Reg8(H)),
        0x65 => Ld8(Reg8(H), Reg8(L)),
        0x66 => Ld8(Reg8(H), MemReg(HL)),
        0x67 => Ld8(Reg8(H), Reg8(A)),
        0x68 => Ld8(Reg8(L), Reg8(B)),
        0x69 => Ld8(Reg8(L), Reg8(C)),
        0x6A => Ld8(Reg8(L), Reg8(D)),
        0x6B => Ld8(Reg8(L), Reg8(E)),
        0x6C => Ld8(Reg8(L), Reg8(H)),
        0x6D => Ld8(Reg8(L), Reg8(L)),
        0x6E => Ld8(Reg8(L), MemReg(HL)),
        0x6F => Ld8(Reg8(L), Reg8(A)),
        0x70 => Ld8(MemReg(HL), Reg8(B)),
        0x71 => Ld8(MemReg(HL), Reg8(C)),
        0x72 => Ld8(MemReg(HL), Reg8(D)),
        0x73 => Ld8(MemReg(HL), Reg8(E)),
        0x74 => Ld8(MemReg(HL), Reg8(H)),
        0x75 => Ld8(MemReg(HL), Reg8(L)),
        0x76 => Halt,
        0x77 => Ld8(MemReg(HL), Reg8(A)),
        0x78 => Ld8(Reg8(A), Reg8(B)),
        0x79 => Ld8(Reg8(A), Reg8(C)),
        0x7A => Ld8(Reg8(A), Reg8(D)),
        0x7B => Ld8(Reg8(A), Reg8(E)),
        0x7C => Ld8(Reg8(A), Reg8(H)),
        0x7D => Ld8(Reg8(A), Reg8(L)),
        0x7E => Ld8(Reg8(A), MemReg(HL)),
        0x7F => Ld8(Reg8(A), Reg8(A)),
        0xA8 => Xor(Reg8(B)),
        0xA9 => Xor(Reg8(C)),
        0xAA => Xor(Reg8(D)),
        0xAB => Xor(Reg8(E)),
        0xAC => Xor(Reg8(H)),
        0xAD => Xor(Reg8(L)),
        0xAF => Xor(Reg8(A)),
        0xC3 => Jp(to_u16(bytes[1], bytes[2]), Cond::None),
        0xC9 => Ret,
        0xCD => Call(to_u16(bytes[1], bytes[2]), Cond::None),
        0xE0 => Ld8(MemImmHigh(bytes[1]), Reg8(A)),
        0xEA => Ld8(MemImm(to_u16(bytes[1], bytes[2])), Reg8(A)),
        0xF0 => Ld8(Reg8(A), MemImmHigh(bytes[1])),
        0xF3 => Di,
        0xFB => Ei,
        0xFE => Cp(Imm8(bytes[1])),

        0xCB => {
            match bytes[1] {
                0x00 => Rlc(Reg8(B)),
                0x01 => Rlc(Reg8(C)),
                0x02 => Rlc(Reg8(D)),
                0x03 => Rlc(Reg8(E)),
                0x04 => Rlc(Reg8(H)),
                0x05 => Rlc(Reg8(L)),
                0x07 => Rlc(Reg8(A)),
                0x08 => Rrc(Reg8(B)),
                0x09 => Rrc(Reg8(C)),
                0x0A => Rrc(Reg8(D)),
                0x0B => Rrc(Reg8(E)),
                0x0C => Rrc(Reg8(H)),
                0x0D => Rrc(Reg8(L)),
                0x0F => Rrc(Reg8(A)),
                0x40 => Bit(0, Reg8(B)),
                0x41 => Bit(0, Reg8(C)),
                0x42 => Bit(0, Reg8(D)),
                0x43 => Bit(0, Reg8(E)),
                0x44 => Bit(0, Reg8(H)),
                0x45 => Bit(0, Reg8(L)),
                0x46 => Bit(0, MemReg(HL)),
                0x47 => Bit(0, Reg8(A)),
                0x48 => Bit(1, Reg8(B)),
                0x49 => Bit(1, Reg8(C)),
                0x4A => Bit(1, Reg8(D)),
                0x4B => Bit(1, Reg8(E)),
                0x4C => Bit(1, Reg8(H)),
                0x4D => Bit(1, Reg8(L)),
                0x4E => Bit(1, MemReg(HL)),
                0x4F => Bit(1, Reg8(A)),
                0x50 => Bit(2, Reg8(B)),
                0x51 => Bit(2, Reg8(C)),
                0x52 => Bit(2, Reg8(D)),
                0x53 => Bit(2, Reg8(E)),
                0x54 => Bit(2, Reg8(H)),
                0x55 => Bit(2, Reg8(L)),
                0x56 => Bit(2, MemReg(HL)),
                0x57 => Bit(2, Reg8(A)),
                0x58 => Bit(3, Reg8(B)),
                0x59 => Bit(3, Reg8(C)),
                0x5A => Bit(3, Reg8(D)),
                0x5B => Bit(3, Reg8(E)),
                0x5C => Bit(3, Reg8(H)),
                0x5D => Bit(3, Reg8(L)),
                0x5E => Bit(3, MemReg(HL)),
                0x5F => Bit(3, Reg8(A)),
                0x60 => Bit(4, Reg8(B)),
                0x61 => Bit(4, Reg8(C)),
                0x62 => Bit(4, Reg8(D)),
                0x63 => Bit(4, Reg8(E)),
                0x64 => Bit(4, Reg8(H)),
                0x65 => Bit(4, Reg8(L)),
                0x66 => Bit(4, MemReg(HL)),
                0x67 => Bit(4, Reg8(A)),
                0x68 => Bit(5, Reg8(B)),
                0x69 => Bit(5, Reg8(C)),
                0x6A => Bit(5, Reg8(D)),
                0x6B => Bit(5, Reg8(E)),
                0x6C => Bit(5, Reg8(H)),
                0x6D => Bit(5, Reg8(L)),
                0x6E => Bit(5, MemReg(HL)),
                0x6F => Bit(5, Reg8(A)),
                0x70 => Bit(6, Reg8(B)),
                0x71 => Bit(6, Reg8(C)),
                0x72 => Bit(6, Reg8(D)),
                0x73 => Bit(6, Reg8(E)),
                0x74 => Bit(6, Reg8(H)),
                0x75 => Bit(6, Reg8(L)),
                0x76 => Bit(6, MemReg(HL)),
                0x77 => Bit(6, Reg8(A)),
                0x78 => Bit(7, Reg8(B)),
                0x79 => Bit(7, Reg8(C)),
                0x7A => Bit(7, Reg8(D)),
                0x7B => Bit(7, Reg8(E)),
                0x7C => Bit(7, Reg8(H)),
                0x7D => Bit(7, Reg8(L)),
                0x7E => Bit(7, MemReg(HL)),
                0x7F => Bit(7, Reg8(A)),
                0x80 => Res(0, Reg8(B)),
                0x81 => Res(0, Reg8(C)),
                0x82 => Res(0, Reg8(D)),
                0x83 => Res(0, Reg8(E)),
                0x84 => Res(0, Reg8(H)),
                0x85 => Res(0, Reg8(L)),
                0x86 => Res(0, MemReg(HL)),
                0x87 => Res(0, Reg8(A)),
                0x88 => Res(1, Reg8(B)),
                0x89 => Res(1, Reg8(C)),
                0x8A => Res(1, Reg8(D)),
                0x8B => Res(1, Reg8(E)),
                0x8C => Res(1, Reg8(H)),
                0x8D => Res(1, Reg8(L)),
                0x8E => Res(1, MemReg(HL)),
                0x8F => Res(1, Reg8(A)),
                0x90 => Res(2, Reg8(B)),
                0x91 => Res(2, Reg8(C)),
                0x92 => Res(2, Reg8(D)),
                0x93 => Res(2, Reg8(E)),
                0x94 => Res(2, Reg8(H)),
                0x95 => Res(2, Reg8(L)),
                0x96 => Res(2, MemReg(HL)),
                0x97 => Res(2, Reg8(A)),
                0x98 => Res(3, Reg8(B)),
                0x99 => Res(3, Reg8(C)),
                0x9A => Res(3, Reg8(D)),
                0x9B => Res(3, Reg8(E)),
                0x9C => Res(3, Reg8(H)),
                0x9D => Res(3, Reg8(L)),
                0x9E => Res(3, MemReg(HL)),
                0x9F => Res(3, Reg8(A)),
                0xA0 => Res(4, Reg8(B)),
                0xA1 => Res(4, Reg8(C)),
                0xA2 => Res(4, Reg8(D)),
                0xA3 => Res(4, Reg8(E)),
                0xA4 => Res(4, Reg8(H)),
                0xA5 => Res(4, Reg8(L)),
                0xA6 => Res(4, MemReg(HL)),
                0xA7 => Res(4, Reg8(A)),
                0xA8 => Res(5, Reg8(B)),
                0xA9 => Res(5, Reg8(C)),
                0xAA => Res(5, Reg8(D)),
                0xAB => Res(5, Reg8(E)),
                0xAC => Res(5, Reg8(H)),
                0xAD => Res(5, Reg8(L)),
                0xAE => Res(5, MemReg(HL)),
                0xAF => Res(5, Reg8(A)),
                0xB0 => Res(6, Reg8(B)),
                0xB1 => Res(6, Reg8(C)),
                0xB2 => Res(6, Reg8(D)),
                0xB3 => Res(6, Reg8(E)),
                0xB4 => Res(6, Reg8(H)),
                0xB5 => Res(6, Reg8(L)),
                0xB6 => Res(6, MemReg(HL)),
                0xB7 => Res(6, Reg8(A)),
                0xB8 => Res(7, Reg8(B)),
                0xB9 => Res(7, Reg8(C)),
                0xBA => Res(7, Reg8(D)),
                0xBB => Res(7, Reg8(E)),
                0xBC => Res(7, Reg8(H)),
                0xBD => Res(7, Reg8(L)),
                0xBE => Res(7, MemReg(HL)),
                0xBF => Res(7, Reg8(A)),
                0xC0 => Set(0, Reg8(B)),
                0xC1 => Set(0, Reg8(C)),
                0xC2 => Set(0, Reg8(D)),
                0xC3 => Set(0, Reg8(E)),
                0xC4 => Set(0, Reg8(H)),
                0xC5 => Set(0, Reg8(L)),
                0xC6 => Set(0, MemReg(HL)),
                0xC7 => Set(0, Reg8(A)),
                0xC8 => Set(1, Reg8(B)),
                0xC9 => Set(1, Reg8(C)),
                0xCA => Set(1, Reg8(D)),
                0xCB => Set(1, Reg8(E)),
                0xCC => Set(1, Reg8(H)),
                0xCD => Set(1, Reg8(L)),
                0xCE => Set(1, MemReg(HL)),
                0xCF => Set(1, Reg8(A)),
                0xD0 => Set(2, Reg8(B)),
                0xD1 => Set(2, Reg8(C)),
                0xD2 => Set(2, Reg8(D)),
                0xD3 => Set(2, Reg8(E)),
                0xD4 => Set(2, Reg8(H)),
                0xD5 => Set(2, Reg8(L)),
                0xD6 => Set(2, MemReg(HL)),
                0xD7 => Set(2, Reg8(A)),
                0xD8 => Set(3, Reg8(B)),
                0xD9 => Set(3, Reg8(C)),
                0xDA => Set(3, Reg8(D)),
                0xDB => Set(3, Reg8(E)),
                0xDC => Set(3, Reg8(H)),
                0xDD => Set(3, Reg8(L)),
                0xDE => Set(3, MemReg(HL)),
                0xDF => Set(3, Reg8(A)),
                0xE0 => Set(4, Reg8(B)),
                0xE1 => Set(4, Reg8(C)),
                0xE2 => Set(4, Reg8(D)),
                0xE3 => Set(4, Reg8(E)),
                0xE4 => Set(4, Reg8(H)),
                0xE5 => Set(4, Reg8(L)),
                0xE6 => Set(4, MemReg(HL)),
                0xE7 => Set(4, Reg8(A)),
                0xE8 => Set(5, Reg8(B)),
                0xE9 => Set(5, Reg8(C)),
                0xEA => Set(5, Reg8(D)),
                0xEB => Set(5, Reg8(E)),
                0xEC => Set(5, Reg8(H)),
                0xED => Set(5, Reg8(L)),
                0xEE => Set(5, MemReg(HL)),
                0xEF => Set(5, Reg8(A)),
                0xF0 => Set(6, Reg8(B)),
                0xF1 => Set(6, Reg8(C)),
                0xF2 => Set(6, Reg8(D)),
                0xF3 => Set(6, Reg8(E)),
                0xF4 => Set(6, Reg8(H)),
                0xF5 => Set(6, Reg8(L)),
                0xF6 => Set(6, MemReg(HL)),
                0xF7 => Set(6, Reg8(A)),
                0xF8 => Set(7, Reg8(B)),
                0xF9 => Set(7, Reg8(C)),
                0xFA => Set(7, Reg8(D)),
                0xFB => Set(7, Reg8(E)),
                0xFC => Set(7, Reg8(H)),
                0xFD => Set(7, Reg8(L)),
                0xFE => Set(7, MemReg(HL)),
                0xFF => Set(7, Reg8(A)),
                _ => return None,
            }
        }

        _ => return None,
    };

    Some(inst)
}

fn to_u16(low: u8, high: u8) -> u16 {
    ((high as u16) << 8) | low as u16
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::Memory;
    use std::fmt::{Debug, UpperHex};
    use std::mem;

    fn setup(rom: Vec<u8>) -> (Cpu, Cpu) {
        let mut cpu = Cpu::new(rom.into_boxed_slice(), Memory::new());
        cpu.reg_pc.set(0);
        (cpu.clone(), cpu)
    }

    /// Check if the actual and expected results are the same, pretty-printing any differences, and
    /// panicking (failing the test) if there are any differences.
    fn check_diff(actual: &Cpu, expected: &Cpu) {
        let mut same = true;
        same &= diff_hex("AF register", &actual.reg_af.get(), &expected.reg_af.get());
        same &= diff_hex("BC register", &actual.reg_bc.get(), &expected.reg_bc.get());
        same &= diff_hex("DE register", &actual.reg_de.get(), &expected.reg_de.get());
        same &= diff_hex("HL register", &actual.reg_hl.get(), &expected.reg_hl.get());
        same &= diff_hex("SP register", &actual.reg_sp.get(), &expected.reg_sp.get());
        same &= diff_hex("PC register", &actual.reg_pc.get(), &expected.reg_pc.get());
        same &= diff("cycles", &actual.cycles, &expected.cycles);
        same &= diff(
            "interrupts_enabled",
            &actual.interrupts_enabled,
            &expected.interrupts_enabled
        );
        same &= diff(
            "pending_disable_interrupts",
            &actual.pending_disable_interrupts,
            &expected.pending_disable_interrupts
        );
        same &= diff(
            "pending_enable_interrupts",
            &actual.pending_enable_interrupts,
            &expected.pending_enable_interrupts
        );

        let actual_mem = actual.memory.mem.iter();
        let expected_mem = expected.memory.mem.iter();
        for (i, (actual_cell, expected_cell)) in actual_mem.zip(expected_mem).enumerate() {
            let name = format!("memory location 0x{:02X}", i);
            same &= diff_hex(&name, actual_cell, expected_cell);
        }

        let actual_rom = actual.rom.iter();
        let expected_rom = expected.rom.iter();
        for (i, (actual_cell, expected_cell)) in actual_rom.zip(expected_rom).enumerate() {
            let name = format!("ROM location 0x{:02X}", i);
            same &= diff_hex(&name, actual_cell, expected_cell);
        }

        if !same {
            panic!("actual and expected results differ");
        }
    }

    /// Returns whether the actual and expected numbers are the same. Pretty-prints the numbers in
    /// hex if they differ.
    fn diff_hex<T: Debug + Eq + UpperHex>(name: &str, actual: &T, expected: &T) -> bool {
        let same = actual == expected;
        if !same {
            let width = mem::size_of::<T>() * 2; // Number of hex digits for type T.
            println!("\ndifference in {}:", name);
            println!("  actual:   0x{0:01$X} ({0:?})", actual, width);
            println!("  expected: 0x{0:01$X} ({0:?})", expected, width);
        }
        same
    }

    /// Returns whether the actual and expected values are the same. Pretty-prints the values if
    /// they differ.
    fn diff<T: Debug + Eq>(name: &str, actual: &T, expected: &T) -> bool {
        let same = actual == expected;
        if !same {
            println!("\ndifference in {}:", name);
            println!("  actual:   {:?}", actual);
            println!("  expected: {:?}", expected);
        }
        same
    }

    #[test]
    fn test_nop() {
        let (mut actual, mut expected) = setup(vec![0x00]); // nop
        actual.step();

        expected.set_reg_16(Regs_16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_reg_8() {
        let (mut actual, mut expected) = setup(vec![
            0x3E, 0x13, // ld a, 0x13
            0x06, 0x42, // ld b, 0x42
        ]);
        actual.set_reg_8(Regs_8::A, 0);
        actual.set_reg_8(Regs_8::B, 0);
        actual.step_n(2);

        expected.set_reg_8(Regs_8::A, 0x13);
        expected.set_reg_8(Regs_8::B, 0x42);
        expected.set_reg_16(Regs_16::PC, 4);
        expected.cycles = 16;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_reg_16() {
        let (mut actual, mut expected) = setup(vec![
            0x11, 0x34, 0x12, // ld de, 0x1234
        ]);
        actual.set_reg_16(Regs_16::DE, 0);
        actual.step();

        expected.set_reg_16(Regs_16::DE, 0x1234);
        expected.set_reg_16(Regs_16::PC, 3);
        expected.cycles = 12;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_jp() {
        let (mut actual, mut expected) = setup(vec![
            0xC3, 0x34, 0x12, // jp 0x1234
        ]);
        actual.step();

        expected.set_reg_16(Regs_16::PC, 0x1234);
        expected.cycles = 16;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_di() {
        let (mut actual, mut expected) = setup(vec![
            0xF3, 0x00, // di; nop
        ]);
        actual.interrupts_enabled = true;
        actual.step(); // di

        // After DI, interrupts still aren't disabled until the next instruction executes.
        expected.interrupts_enabled = true;
        expected.pending_disable_interrupts = true;
        expected.set_reg_16(Regs_16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);

        println!("# Step 2");
        actual.step(); // nop

        // Now the instruction after DI has executed, so DI takes effect.
        expected.interrupts_enabled = false;
        expected.pending_disable_interrupts = false;
        expected.set_reg_16(Regs_16::PC, 2);
        expected.cycles = 8;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_ei() {
        let (mut actual, mut expected) = setup(vec![
            0xFB, 0x00, // ei; nop
        ]);
        actual.interrupts_enabled = false;
        actual.step(); // ei

        // After EI, interrupts still aren't disabled until the next instruction executes.
        expected.interrupts_enabled = false;
        expected.pending_enable_interrupts = true;
        expected.set_reg_16(Regs_16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);

        println!("# Step 2");
        actual.step(); // nop

        // Now the instruction after EI has executed, so EI takes effect.
        expected.interrupts_enabled = true;
        expected.pending_enable_interrupts = false;
        expected.set_reg_16(Regs_16::PC, 2);
        expected.cycles = 8;
        check_diff(&actual, &expected);
    }
}
