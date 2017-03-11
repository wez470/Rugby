use memory::Memory;
use reg_16::Register;
use self::inst::{Cond, Inst, Operand8, Operand16};

mod inst;

#[derive(Clone, Copy, Debug)]
pub enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Clone, Copy, Debug)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
enum Flag {
    Zero = 7,
    Sub = 6,
    HalfCarry = 5,
    Carry = 4,
}

#[derive(Clone)]
pub struct Cpu {
    /// Register containing the flags register and register 'A'
    reg_af: Register,

    /// Register containing registers 'B' and 'C'
    reg_bc: Register,

    /// Register containing registers 'D' and 'E'
    reg_de: Register,

    /// Register containing registers 'H' and 'L'
    reg_hl: Register,

    /// Register that contains the stack pointer
    reg_sp: Register,

    /// Register that contains the program counter
    reg_pc: Register,

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
            reg_af: Register::default(),
            reg_bc: Register::default(),
            reg_de: Register::default(),
            reg_hl: Register::default(),
            reg_sp: Register::default(),
            reg_pc: Register::default(),
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

    pub fn step_n(&mut self, steps: usize) {
        for _ in 0..steps {
            self.step();
        }
    }

    pub fn step(&mut self) {
        let pending_enable_interrupts = self.pending_enable_interrupts;
        let pending_disable_interrupts = self.pending_disable_interrupts;
        self.pending_enable_interrupts = false;
        self.pending_disable_interrupts = false;

        self.base_pc = self.reg_pc.get() as usize;
        let opcode = self.rom[self.base_pc];
        let instruction_len = inst::INSTRUCTION_LENGTH[opcode as usize];
        self.reg_pc.inc(instruction_len as i8);

        self.cycles += if opcode == 0xCB {
            let opcode_after_cb = self.rom[self.base_pc + 1];
            inst::PREFIX_CB_BASE_CYCLES[opcode_after_cb as usize]
        } else {
            inst::BASE_CYCLES[opcode as usize]
        };

        print!("{:04X}:", self.base_pc);
        for &byte in &self.rom[self.base_pc .. (self.base_pc + instruction_len)] {
            print!(" {:02X}", byte);
        }

        let inst = Inst::from_bytes(&self.rom[self.base_pc..(self.base_pc + instruction_len)]);
        println!("\t\t(decoded: {:?})", inst);

        self.execute(inst);

        if pending_enable_interrupts {
            self.interrupts_enabled = true;
        }

        if pending_disable_interrupts {
            self.interrupts_enabled = false;
        }
    }

    fn execute(&mut self, inst: Inst) {
        match inst {
            Inst::Nop => {},
            Inst::Stop => println!(" Unimplemented"),
            Inst::Halt => println!(" Unimplemented"),
            Inst::Di => self.pending_disable_interrupts = true,
            Inst::Ei => self.pending_enable_interrupts = true,
            Inst::Jp(loc, cond) => self.jump(loc, cond),
            Inst::Jr(offset, cond) => self.jump_relative(offset, cond),
            Inst::Call(fn_addr, cond) => self.call(fn_addr, cond),
            Inst::Rst(addr) => self.call_restart(addr),
            Inst::Ret(cond) => self.ret(cond),
            Inst::Reti => {
                self.interrupts_enabled = true;
                self.ret(Cond::None);
            }
            Inst::Push(reg) => self.push(reg),
            Inst::Pop(reg) => self.pop(reg),
            Inst::Ld8(dest, src) => self.move_8(dest, src),
            Inst::Ld16(dest, src) => self.move_16(dest, src),
            Inst::LdHlSp(offset) => self.load_stack_addr_into_reg(offset, Reg16::HL),
            Inst::Inc8(n) => self.inc_8(n),
            Inst::Dec8(n) => self.dec_8(n),
            Inst::Inc16(n) => self.inc_16(n),
            Inst::Dec16(n) => self.dec_16(n),
            Inst::AddA(n) => self.add_accum(n),
            Inst::AddHl(n) => self.add_hl(n),
            Inst::AddSp(offset) => self.load_stack_addr_into_reg(offset, Reg16::SP),
            Inst::AdcA(n) => self.add_accum_with_carry(n),
            Inst::Sub(n) => self.sub_accum(n),
            Inst::SbcA(n) => self.sub_accum_with_carry(n),
            Inst::And(n) => self.and_accum(n),
            Inst::Xor(n) => self.xor_accum(n),
            Inst::Or(n) => self.or_accum(n),
            Inst::Cp(n) => self.compare_accum(n),
            Inst::Rlc(n) => self.rotate_left_circular(n),
            Inst::Rl(n) => self.rotate_left(n),
            Inst::Rrc(n) => self.rotate_right_circular(n),
            Inst::Rr(n) => self.rotate_right(n),
            Inst::Rlca => {
                self.rotate_left_circular(Operand8::Reg8(Reg8::A));
                self.set_flag(Flag::Zero, false);
            }
            Inst::Rla => {
                self.rotate_left(Operand8::Reg8(Reg8::A));
                self.set_flag(Flag::Zero, false);
            }
            Inst::Rrca => {
                self.rotate_right_circular(Operand8::Reg8(Reg8::A));
                self.set_flag(Flag::Zero, false);
            }
            Inst::Rra => {
                self.rotate_right(Operand8::Reg8(Reg8::A));
                self.set_flag(Flag::Zero, false);
            }
            Inst::Sla(n) => self.shift_left_arith(n),
            Inst::Sra(n) => self.shift_right_arith(n),
            Inst::Srl(n) => self.shift_right_logical(n),
            Inst::Swap(n) => self.swap(n),
            Inst::Bit(bit, n) => self.test_bit(bit, n),
            Inst::Res(bit, n) => self.reset_bit(bit, n),
            Inst::Set(bit, n) => self.set_bit(bit, n),
            Inst::Daa => println!(" Unimplemented"),
            Inst::Cpl => self.complement_accum(),
            Inst::Ccf => self.complement_carry_flag(),
            Inst::Scf => self.set_carry_flag(),
            Inst::Invalid(opcode) => panic!("tried to execute invalid opcode {:#X}", opcode),
        }
    }

    /// The `Inst::Jp` instruction.
    ///
    /// Jump to the specified address if the condition is met.
    fn jump(&mut self, addr: Operand16, cond: Cond) {
        let addr_val = self.get_operand_16(addr);
        if self.check_cond_and_update_cycles(cond) {
            self.reg_pc.set(addr_val);
        }
    }

    /// The `Inst::Jr` instruction.
    ///
    /// Increment the program counter by the given offset if the condition is met.
    fn jump_relative(&mut self, offset: i8, cond: Cond) {
        if self.check_cond_and_update_cycles(cond) {
            self.reg_pc.inc(offset);
        }
    }

    /// The `Inst::Call` instruction.
    ///
    /// Call a subroutine if the condition is met.
    fn call(&mut self, fn_addr: u16, cond: Cond) {
        if self.check_cond_and_update_cycles(cond) {
            // The return address is the address of the instruction after the call.
            let return_addr = self.reg_pc.get();
            self.push_stack(return_addr);
            self.reg_pc.set(fn_addr);
        }
    }

    /// The `Inst::Rst` instruction.
    ///
    /// Call the "restart" function at the given address.
    fn call_restart(&mut self, addr: u8) {
        let return_addr = self.reg_pc.get();
        self.push_stack(return_addr);
        self.reg_pc.set(addr as u16);
    }

    /// The `Inst::Ret` instruction.
    fn ret(&mut self, cond: Cond) {
        if self.check_cond_and_update_cycles(cond) {
            let return_addr = self.pop_stack();
            self.set_reg_16(Reg16::PC, return_addr);
        }
    }

    /// The `Inst::Push` instruction.
    fn push(&mut self, reg: Reg16) {
        let val = self.get_reg_16(reg);
        self.push_stack(val);
    }

    /// The `Inst::Pop` instruction.
    fn pop(&mut self, reg: Reg16) {
        let val = self.pop_stack();
        self.set_reg_16(reg, val);
    }

    /// The 8-bit `Inst::Ld` instruction.
    ///
    /// Move 8 bits from `src` and store them into `dest`.
    fn move_8(&mut self, dest: Operand8, src: Operand8) {
        let val = self.get_operand_8(src);
        self.set_operand_8(dest, val);
    }

    /// The 16-bit `Inst::Ld` instruction.
    ///
    /// Move 16 bits from `src` and store them into `dest`.
    fn move_16(&mut self, dest: Operand16, src: Operand16) {
        let val = self.get_operand_16(src);
        self.set_operand_16(dest, val);
    }

    /// The `Inst::LdHlSp` and `Inst::AddSp` instructions.
    ///
    /// Loads the stack pointer plus an 8-bit signed value into the specified register.
    fn load_stack_addr_into_reg(&mut self, offset: i8, dest: Reg16) {
        let sp = self.get_reg_16(Reg16::SP);
        let val = (sp as i32 + offset as i32) as u16;
        self.set_reg_16(dest, val);
        self.set_flag(Flag::Zero, false);
        self.set_flag(Flag::Sub, false);
        // TODO(wcarlson): Potential bugs in this section. Unsure if these implementations are
        // correct
        if offset >= 0 {
            self.set_flag(Flag::HalfCarry, get_add_half_carry((sp & 0xFF) as u8, offset as u8));
            self.set_flag(Flag::Carry, (sp & 0xFF) as u16 + offset as u16 > 0xFF);
        } else {
            self.set_flag(Flag::HalfCarry, get_sub_half_carry((val & 0xFF) as u8, (sp & 0xFF) as u8));
            self.set_flag(Flag::Carry, (val & 0xFF) <= (sp & 0xFF)); // Uncertain about this
        }
    }

    /// The 8-bit `Inst::Inc` instruction.
    fn inc_8(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val.wrapping_add(1);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, get_add_half_carry(old_val, 1));
    }

    /// The 8-bit `Inst::Dec` instruction.
    fn dec_8(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val.wrapping_sub(1);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, true);
        self.set_flag(Flag::HalfCarry, get_sub_half_carry(old_val, 1));
    }

    /// The 16-bit `Inst::Inc` instruction.
    fn inc_16(&mut self, n: Operand16) {
        let val = self.get_operand_16(n).wrapping_add(1);
        self.set_operand_16(n, val);
    }

    /// The 16-bit `Inst::Dec` instruction.
    fn dec_16(&mut self, n: Operand16) {
        let val = self.get_operand_16(n).wrapping_sub(1);
        self.set_operand_16(n, val);
    }

    /// The `Inst::AddA` instruction
    fn add_accum(&mut self, n: Operand8) {
        let accum = self.get_reg_8(Reg8::A);
        let n_val = self.get_operand_8(n);
        let (new_accum, carry) = accum.overflowing_add(n_val);
        self.set_reg_8(Reg8::A, new_accum);
        self.set_flag(Flag::Zero, new_accum == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, get_add_half_carry(accum, n_val));
        self.set_flag(Flag::Carry, carry);
    }

    /// The `Inst::AddHl` instruction
    fn add_hl(&mut self, n: Operand16) {
        let old_hl = self.get_reg_16(Reg16::HL);
        let n_val = self.get_operand_16(n);
        let (new_hl, carry) = old_hl.overflowing_add(n_val);
        self.set_reg_16(Reg16::HL, new_hl);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, get_add_half_carry_high(old_hl, n_val));
        self.set_flag(Flag::Carry, carry);
    }

    /// The `Inst::AdcA` instruction
    fn add_accum_with_carry(&mut self, n: Operand8) {
        let accum = self.get_reg_8(Reg8::A);
        let n_val = self.get_operand_8(n);
        let carry_val = self.get_flag(Flag::Carry) as u8;
        let (midway_accum, midway_carry) = accum.overflowing_add(n_val);
        let (final_accum, final_carry) = midway_accum.overflowing_add(carry_val);
        self.set_flag(Flag::Zero, final_accum == 0);
        self.set_flag(Flag::Sub, false);
        let half_carry = get_add_half_carry(accum, n_val) || get_add_half_carry(midway_accum, carry_val);
        self.set_flag(Flag::HalfCarry, half_carry);
        self.set_flag(Flag::Carry, midway_carry || final_carry);
    }

    /// The `Inst::Sub` instruction
    fn sub_accum(&mut self, n: Operand8) {
        let accum = self.get_reg_8(Reg8::A);
        let n_val = self.get_operand_8(n);
        let (new_accum, carry) = accum.overflowing_sub(n_val);
        self.set_reg_8(Reg8::A, new_accum);
        self.set_flag(Flag::Zero, new_accum == 0);
        self.set_flag(Flag::Sub, true);
        self.set_flag(Flag::HalfCarry, get_sub_half_carry(accum, n_val));
        self.set_flag(Flag::Carry, carry);
    }

    /// The `Inst::SbcA` instruction
    fn sub_accum_with_carry(&mut self, n: Operand8) {
        let accum = self.get_reg_8(Reg8::A);
        let n_val = self.get_operand_8(n);
        let carry_val = self.get_flag(Flag::Carry) as u8;
        let (midway_accum, midway_carry) = accum.overflowing_sub(n_val);
        let (final_accum, final_carry) = midway_accum.overflowing_sub(carry_val);
        self.set_flag(Flag::Zero, final_accum == 0);
        self.set_flag(Flag::Sub, true);
        let half_carry = get_sub_half_carry(accum, n_val) || get_sub_half_carry(midway_accum, carry_val);
        self.set_flag(Flag::HalfCarry, half_carry);
        self.set_flag(Flag::Carry, midway_carry || final_carry);
    }

    /// The `Inst::And` instruction.
    fn and_accum(&mut self, n: Operand8) {
        let result = self.reg_af.high & self.get_operand_8(n);
        self.reg_af.high = result;
        self.set_flag(Flag::Zero, result == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, true);
        self.set_flag(Flag::Carry, false);
    }

    /// The `Inst::Xor` instruction.
    fn xor_accum(&mut self, n: Operand8) {
        let result = self.reg_af.high ^ self.get_operand_8(n);
        self.reg_af.high = result;
        self.set_flag(Flag::Zero, result == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, true);
        self.set_flag(Flag::Carry, false);
    }

    /// The `Inst::Or` instruction.
    fn or_accum(&mut self, n: Operand8) {
        let result = self.reg_af.high | self.get_operand_8(n);
        self.reg_af.high = result;
        self.set_flag(Flag::Zero, result == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, false);
    }

    /// The `Inst::Cp` instruction.
    fn compare_accum(&mut self, n: Operand8) {
        let left = self.reg_af.high;
        let right = self.get_operand_8(n);
        self.set_flag(Flag::Zero, left == right);
        self.set_flag(Flag::Sub, true);
        self.set_flag(Flag::HalfCarry, get_sub_half_carry(left, right));
        self.set_flag(Flag::Carry, left < right);
    }

    /// The `Inst::Rlc` instruction.
    fn rotate_left_circular(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val.rotate_left(1);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x80 != 0);
    }

    /// The `Inst::Rl` instruction.
    fn rotate_left(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let old_carry = self.get_flag(Flag::Carry) as u8;
        let new_val = (old_val << 1) | old_carry;
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x80 != 0);
    }

    /// The `Inst::Rrc` instruction.
    fn rotate_right_circular(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val.rotate_right(1);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x01 != 0);
    }

    /// The `Inst::Rr` instruction.
    fn rotate_right(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let old_carry = self.get_flag(Flag::Carry) as u8;
        let new_val = (old_carry << 7) | (old_val >> 1);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x01 != 0);
    }

    /// The `Inst::Sla` instruction.
    fn shift_left_arith(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val << 1;
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x80 != 0);
    }

    /// The `Inst::Sra` instruction.
    fn shift_right_arith(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let high_bit = old_val & 0x80;
        let new_val = (old_val >> 1) | high_bit;
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x01 != 0);
    }

    /// The `Inst::Srl` instruction.
    fn shift_right_logical(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val >> 1;
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x01 != 0);
    }

    /// The `Inst::Swap` instruction.
    fn swap(&mut self, n: Operand8) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val.rotate_right(4);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, false);
    }

    /// The `Inst::Bit` instruction.
    fn test_bit(&mut self, bit: u8, n: Operand8) {
        let is_zero = self.get_operand_8(n) & (1 << bit) == 0;
        self.set_flag(Flag::Zero, is_zero);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, true);
    }

    /// The `Inst::Res` instruction.
    fn reset_bit(&mut self, bit: u8, n: Operand8) {
        let val = self.get_operand_8(n) & !(1 << bit);
        self.set_operand_8(n, val);
    }

    /// The `Inst::Set` instruction.
    fn set_bit(&mut self, bit: u8, n: Operand8) {
        let val = self.get_operand_8(n) | (1 << bit);
        self.set_operand_8(n, val);
    }

    /// The `Inst::Cpl` instruction.
    fn complement_accum(&mut self) {
        self.reg_af.high = !self.reg_af.high;
        self.set_flag(Flag::Sub, true);
        self.set_flag(Flag::HalfCarry, true);
    }

    /// The `Inst::Ccf` instruction.
    fn complement_carry_flag(&mut self) {
        let carry = !self.get_flag(Flag::Carry);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, carry);
    }

    /// The `Inst::Scf` instruction.
    fn set_carry_flag(&mut self) {
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, true);
    }

    /// If the given condition is met, increment the cycle count accordingly and return true.
    fn check_cond_and_update_cycles(&mut self, cond: Cond) -> bool {
        let condition_met = self.is_cond_met(cond);
        if condition_met {
            let opcode = self.rom[self.base_pc];
            self.cycles += inst::CONDITIONAL_CYCLES[opcode as usize];
        }
        condition_met
    }

    /// Return true if the given condition is met.
    fn is_cond_met(&self, cond: Cond) -> bool {
        match cond {
            Cond::None => true,
            Cond::Zero => self.get_flag(Flag::Zero),
            Cond::NotZero => !self.get_flag(Flag::Zero),
            Cond::Carry => self.get_flag(Flag::Carry),
            Cond::NotCarry => !self.get_flag(Flag::Carry),
        }
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
        ((high as u16) << 8) | low as u16
    }

    fn set_flag(&mut self, flag: Flag, val: bool) {
        self.reg_af.set_bit(flag as u8, val);
    }

    fn get_flag(&self, flag: Flag) -> bool {
        self.reg_af.is_bit_set(flag as u8)
    }

    /// Get the value of the given 8-bit operand.
    ///
    /// NOTE: Accessing some operands has side effects, so you should not call this twice on a
    /// given operand.
    fn get_operand_8(&mut self, src: Operand8) -> u8 {
        match src {
            Operand8::Imm8(val) => val,
            Operand8::Reg8(reg) => self.get_reg_8(reg),
            Operand8::MemImm(loc) => self.memory.mem[loc as usize],
            Operand8::MemReg(reg) => self.memory.mem[self.get_reg_16(reg) as usize],
            Operand8::MemHighImm(offset) => self.memory.mem[0xFF00 | offset as usize],
            Operand8::MemHighC => {
                let offset = self.get_reg_8(Reg8::C);
                self.memory.mem[0xFF00 | offset as usize]
            }
            Operand8::MemHlPostInc => {
                let hl = self.get_reg_16(Reg16::HL);
                let val = self.memory.mem[hl as usize];
                self.set_reg_16(Reg16::HL, hl.wrapping_add(1));
                val
            }
            Operand8::MemHlPostDec => {
                let hl = self.get_reg_16(Reg16::HL);
                let val = self.memory.mem[hl as usize];
                self.set_reg_16(Reg16::HL, hl.wrapping_sub(1));
                val
            }
        }
    }

    /// Set the value of the given 8-bit operand.
    ///
    /// NOTE: Accessing some operands has side effects, so you should not call this twice on a
    /// given operand.
    fn set_operand_8(&mut self, dest: Operand8, val: u8) {
        match dest {
            Operand8::Imm8(_) => panic!("Attempt to store to an 8-bit immediate value"),
            Operand8::Reg8(reg) => self.set_reg_8(reg, val),
            Operand8::MemImm(loc) => self.memory.mem[loc as usize] = val,
            Operand8::MemReg(reg) => self.memory.mem[self.get_reg_16(reg) as usize] = val,
            Operand8::MemHighImm(offset) => self.memory.mem[0xFF00 | offset as usize] = val,
            Operand8::MemHighC => {
                let offset = self.get_reg_8(Reg8::C);
                self.memory.mem[0xFF00 | offset as usize] = val;
            }
            Operand8::MemHlPostInc => {
                let hl = self.get_reg_16(Reg16::HL);
                self.memory.mem[hl as usize] = val;
                self.set_reg_16(Reg16::HL, hl.wrapping_add(1));
            }
            Operand8::MemHlPostDec => {
                let hl = self.get_reg_16(Reg16::HL);
                self.memory.mem[hl as usize] = val;
                self.set_reg_16(Reg16::HL, hl.wrapping_sub(1));
            }
        }
    }

    fn get_operand_16(&self, src: Operand16) -> u16 {
        match src {
            Operand16::Imm16(val) => val,
            Operand16::Reg16(reg) => self.get_reg_16(reg),
            Operand16::MemImm16(_) => panic!("no gameboy CPU instruction actually uses this"),
        }
    }

    fn set_operand_16(&mut self, dest: Operand16, val: u16) {
        match dest {
            Operand16::Imm16(_) => panic!("Attempt to store to a 16-bit immediate value"),
            Operand16::Reg16(reg) => self.set_reg_16(reg, val),
            Operand16::MemImm16(_) => unimplemented!(),
        }
    }

    fn set_reg_8(&mut self, reg: Reg8, val: u8) {
        match reg {
            Reg8::A => self.reg_af.high = val,
            Reg8::B => self.reg_bc.high = val,
            Reg8::C => self.reg_bc.low = val,
            Reg8::D => self.reg_de.high = val,
            Reg8::E => self.reg_de.low = val,
            Reg8::H => self.reg_hl.high = val,
            Reg8::L => self.reg_hl.low = val,
        }
    }

    fn get_reg_8(&self, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => self.reg_af.high,
            Reg8::B => self.reg_bc.high,
            Reg8::C => self.reg_bc.low,
            Reg8::D => self.reg_de.high,
            Reg8::E => self.reg_de.low,
            Reg8::H => self.reg_hl.high,
            Reg8::L => self.reg_hl.low,
        }
    }

    fn set_reg_16(&mut self, reg: Reg16, val: u16) {
        match reg {
            Reg16::AF => self.reg_af.set(val),
            Reg16::BC => self.reg_bc.set(val),
            Reg16::DE => self.reg_de.set(val),
            Reg16::HL => self.reg_hl.set(val),
            Reg16::SP => self.reg_sp.set(val),
            Reg16::PC => self.reg_pc.set(val),
        }
    }

    fn get_reg_16(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => self.reg_af.get(),
            Reg16::BC => self.reg_bc.get(),
            Reg16::DE => self.reg_de.get(),
            Reg16::HL => self.reg_hl.get(),
            Reg16::SP => self.reg_sp.get(),
            Reg16::PC => self.reg_pc.get(),
        }
    }
}

/// Returns true if `left + right` should set the half-carry flag, i.e. it requires a carry
/// from bit 3 into bit 4.
fn get_add_half_carry(left: u8, right: u8) -> bool {
    (left & 0xf) + (right & 0xf) > 0xf
}

/// Returns true if `left - right` should set the half-carry flag, i.e. it requires a borrow
/// from bit 4 into bit 3.
fn get_sub_half_carry(left: u8, right: u8) -> bool {
    (left & 0xf) < (right & 0xf)
}

/// Returns true if `left + right` should set the half-carry flag, i.e. it requires a carry
/// from bit 11 into bit 12.
///
/// This is for 16-bit adds, where the half-carry is set based on the halfway point of the high
/// byte.
fn get_add_half_carry_high(left: u16, right: u16) -> bool {
    (left & 0xfff) + (right & 0xfff) > 0xfff
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

        expected.set_reg_16(Reg16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_reg_8() {
        let (mut actual, mut expected) = setup(vec![
            0x3E, 0x13, // ld a, 0x13
            0x06, 0x42, // ld b, 0x42
        ]);
        actual.set_reg_8(Reg8::A, 0);
        actual.set_reg_8(Reg8::B, 0);
        actual.step_n(2);

        expected.set_reg_8(Reg8::A, 0x13);
        expected.set_reg_8(Reg8::B, 0x42);
        expected.set_reg_16(Reg16::PC, 4);
        expected.cycles = 16;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_reg_16() {
        let (mut actual, mut expected) = setup(vec![
            0x11, 0x34, 0x12, // ld de, 0x1234
        ]);
        actual.set_reg_16(Reg16::DE, 0);
        actual.step();

        expected.set_reg_16(Reg16::DE, 0x1234);
        expected.set_reg_16(Reg16::PC, 3);
        expected.cycles = 12;
        check_diff(&actual, &expected);
    }

    #[test]
    fn test_jp() {
        let (mut actual, mut expected) = setup(vec![
            0xC3, 0x34, 0x12, // jp 0x1234
        ]);
        actual.step();

        expected.set_reg_16(Reg16::PC, 0x1234);
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
        expected.set_reg_16(Reg16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);

        println!("# Step 2");
        actual.step(); // nop

        // Now the instruction after DI has executed, so DI takes effect.
        expected.interrupts_enabled = false;
        expected.pending_disable_interrupts = false;
        expected.set_reg_16(Reg16::PC, 2);
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
        expected.set_reg_16(Reg16::PC, 1);
        expected.cycles = 4;
        check_diff(&actual, &expected);

        println!("# Step 2");
        actual.step(); // nop

        // Now the instruction after EI has executed, so EI takes effect.
        expected.interrupts_enabled = true;
        expected.pending_enable_interrupts = false;
        expected.set_reg_16(Reg16::PC, 2);
        expected.cycles = 8;
        check_diff(&actual, &expected);
    }
}
