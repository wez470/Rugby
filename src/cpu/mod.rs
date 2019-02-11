use log::{debug, info, log_enabled, trace, warn};
use crate::cart::Cart;
use crate::interrupts::Interrupt;
use crate::joypad::Joypad;
use crate::reg_16::Register;
use crate::timer::Timer;
use crate::gpu::Gpu;
use self::inst::{Cond, Inst, Operand16, Operand8};

mod inst;

// TODO: Refactor later to extract memory-related stuff out of the cpu module.
const WORK_RAM_SIZE: usize = 8 * 1024; // 8 KB
const HIGH_RAM_SIZE: usize = 127; // For the address range 0xFF80-0xFFFE (inclusive).

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

/// Represents bit indexes of flags in the flags register.
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
    /// The 16-bit `AF` register, composed of two 8-bit registers:
    ///   * `A`, also known as the accumulator.
    ///   * `F`, the flags register.
    reg_af: Register,

    /// The 16-bit `BC` register, composed of the two general-purpose 8-bit registers `B` and `C`.
    reg_bc: Register,

    /// The 16-bit `DE` register, composed of the two general-purpose 8-bit registers `D` and `E`.
    /// Register containing registers 'D' and 'E'
    reg_de: Register,

    /// The 16-bit `HL` register, composed of the two general-purpose 8-bit registers `H` and `L`.
    reg_hl: Register,

    /// The 16-bit `SP` register, which contains the stack pointer.
    reg_sp: Register,

    /// The 16-bit `PC` register, which contains the program counter.
    reg_pc: Register,

    /// Work RAM internal to the Gameboy, as opposed to external cartridge RAM. Limited to 8 KB in
    /// the original Gameboy.
    work_ram: Box<[u8]>,

    /// High RAM internal to the Gameboy. This is a small range of 127 bytes at 0xFF80-0xFFFE.
    high_ram: Box<[u8]>,

    /// The Gameboy timing registers
    timer: Timer,

    /// The graphics procession unit.
    pub gpu: Gpu,

    /// The player controller hardware.
    pub joypad: Joypad,

    /// Game cartridge.
    pub cart: Cart,

    /// The opcode of the currently-executing instruction.
    current_opcode: u8,

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

    /// The `IF` Interrupt Flags register accessed via I/O port 0xFF0F.
    interrupt_flags_register: u8,

    /// The `IE` Interrupt Enable register accessed via I/O port 0xFFFF.
    interrupt_enable_register: u8,

    /// If the cpu is halted
    halted: bool,

    /// If the cpu is stopped
    stopped: bool,

    /// Symbolic information for more detailed debug output.
    // TODO(solson): Should we find another place to store this?
    pub debug_symbols: Option<crate::wla_symbols::WlaSymbols>,
}

impl Cpu {
    pub fn new(cart: Cart) -> Cpu {
        let mut cpu = Cpu {
            reg_af: Register::default(),
            reg_bc: Register::default(),
            reg_de: Register::default(),
            reg_hl: Register::default(),
            reg_sp: Register::default(),
            reg_pc: Register::default(),
            work_ram: vec![0; WORK_RAM_SIZE].into_boxed_slice(),
            high_ram: vec![0; HIGH_RAM_SIZE].into_boxed_slice(),
            timer: Timer::new(),
            gpu: Gpu::new(),
            joypad: Joypad::new(),
            cart,
            current_opcode: 0,
            cycles: 0,
            interrupts_enabled: false,
            pending_disable_interrupts: false,
            pending_enable_interrupts: false,
            interrupt_flags_register: 1,
            interrupt_enable_register: 0,
            halted: false,
            stopped: false,
            debug_symbols: None,
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

    /// Keep executing instructions until more than the given number of cycles have passed.
    pub fn step_cycles(&mut self, cycles: usize) {
        let mut curr_cycles: usize = 0;
        while curr_cycles < cycles {
            let step_cycles = self.step();
            for inter in self.gpu.step(step_cycles) {
                self.request_interrupt(inter)
            }
            if let Some(inter) = self.timer.step(step_cycles) {
                self.request_interrupt(inter)
            }
            if let Some(inter) = self.joypad.step() {
                self.request_interrupt(inter);
            }
            curr_cycles += step_cycles;
        }
    }

    /// Execute a single instruction. Returns how many cycles it took.
    // TODO(solson): Should calling this directly be avoided, since only `step_cycles` checks for
    // GPU, Timer, and Joypad interurpts?
    pub fn step(&mut self) -> usize {
        let pending_enable_interrupts = self.pending_enable_interrupts;
        let pending_disable_interrupts = self.pending_disable_interrupts;
        self.pending_enable_interrupts = false;
        self.pending_disable_interrupts = false;

        self.handle_interrupts();

        if self.halted {
            return 4;
        }

        // Get the opcode for the current instruction and find the total instruction length.
        let base_pc = self.reg_pc.get();
        self.current_opcode = self.read_mem(base_pc);
        let instruction_len = inst::INSTRUCTION_LENGTH[self.current_opcode as usize];
        self.reg_pc.inc(instruction_len as i8);

        // Read the rest of the bytes of this instruction.
        let mut inst_bytes = [0u8; inst::MAX_INSTRUCTION_LENGTH];
        inst_bytes[0] = self.current_opcode;
        for i in 1..instruction_len {
            inst_bytes[i] = self.read_mem(base_pc.wrapping_add(i as u16));
        }

        // Update clock cycle count based on the current instruction.
        let cycles = if self.current_opcode == 0xCB {
            let opcode_after_cb = inst_bytes[1];
            inst::PREFIX_CB_BASE_CYCLES[opcode_after_cb as usize]
        } else {
            inst::BASE_CYCLES[self.current_opcode as usize]
        };

        // Decode the instruction.
        let inst = Inst::from_bytes(&inst_bytes[..instruction_len]);
        trace!("PC=0x{:04X}: {:?}", base_pc, inst);

        self.execute(inst);

        if pending_enable_interrupts {
            self.interrupts_enabled = true;
        }

        if pending_disable_interrupts {
            self.interrupts_enabled = false;
        }

        cycles
    }

    fn handle_interrupts(&mut self) {
        self.check_interrupt(Interrupt::VerticalBlank);
        self.check_interrupt(Interrupt::LCD);
        self.check_interrupt(Interrupt::Timer);
        self.check_interrupt(Interrupt::Serial);
        if self.interrupt_pending(Interrupt::Joypad) {
            self.stopped = false;
        }
        self.check_interrupt(Interrupt::Joypad);
    }

    fn check_interrupt(&mut self, i: Interrupt) {
        if self.interrupt_pending(i) {
            self.halted = false;
        }
        if self.interrupts_enabled && self.interrupt_pending(i) && self.interrupt_enabled(i) {
            debug!("Handling interrupt {:?}", i);
            // TODO(solson): Use `call` or `call_restart`?
            self.push_stack(self.get_reg_16(Reg16::PC));
            self.set_reg_16(Reg16::PC, i.handler_addr());
            self.reset_interrupt(i);
        }
    }

    fn interrupt_pending(&self, i: Interrupt) -> bool {
        self.is_bit_set_at_location(i as i32, 0xFF0F)
    }

    fn interrupt_enabled(&self, i: Interrupt) -> bool {
        self.is_bit_set_at_location(i as i32, 0xFFFF)
    }

    pub fn request_interrupt(&mut self, i: Interrupt) {
        debug!("Requesting interrupt: {:?}", i);
        self.set_bit_at_location(i as i32, 0xFF0F);
    }

    fn is_bit_set_at_location(&self, bit: i32, addr: u16) -> bool {
        let val = self.read_mem(addr);
        val >> bit & 1 != 0
    }

    fn set_bit_at_location(&mut self, bit: i32, addr: u16) {
        let mut val = self.read_mem(addr);
        val |= 1 << bit;
        self.write_mem(addr, val);
    }

    fn reset_interrupt(&mut self, i: Interrupt) {
        let addr = 0xFF0F;
        let val = self.read_mem(addr);
        let new_val = val & !(1 << (i as i32));
        self.write_mem(addr, new_val);
    }

    fn execute(&mut self, inst: Inst) {
        match inst {
            Inst::Nop => {}
            Inst::Stop => self.stopped = true,
            Inst::Halt => self.halted = true,
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
            Inst::Daa => self.decimal_adjust_accum(),
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
            // TODO(solson): Deduplicate this code with the block in `call`.
            if log_enabled!(log::Level::Trace) {
                if let Some(symbols) = &self.debug_symbols {
                    let rom_addr = crate::wla_symbols::RomAddr { bank: 1, addr: addr_val };
                    match symbols.labels.get(&rom_addr) {
                        Some(name) => trace!("tailcalling function {} at 0x{:04X}", name, addr_val),
                        None => trace!("tailcalling unknown function at 0x{:04X}", addr_val)
                    }
                }
            }

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
            if log_enabled!(log::Level::Trace) {
                if let Some(symbols) = &self.debug_symbols {
                    // TODO(solson): Fix this harded bank number. (It's not exactly clear to me how
                    // to interpret the WLA DX symbol file bank numbers yet.)
                    let rom_addr = crate::wla_symbols::RomAddr { bank: 1, addr: fn_addr };
                    match symbols.labels.get(&rom_addr) {
                        Some(name) => trace!("calling function {} at 0x{:04X}", name, fn_addr),
                        None => trace!("calling unknown function at 0x{:04X}", fn_addr)
                    }
                }
            }

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
        // TODO(solson): Use `call` function?
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
        warn!("executing LdHlSp or AddSp which may be buggy");
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
        self.set_reg_8(Reg8::A, final_accum);
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
        self.set_reg_8(Reg8::A, final_accum);
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
        self.set_flag(Flag::HalfCarry, false);
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

    /// The `Inst::Daa` instruction.
    fn decimal_adjust_accum(&mut self) {
        let half_carry = self.get_flag(Flag::HalfCarry);
        let carry = self.get_flag(Flag::Carry);
        let sub = self.get_flag(Flag::Sub);

        let mut accum = self.get_reg_8(Reg8::A);
        let mut set_carry = false;
        let mut correction = 0;

        if half_carry || (!sub && accum & 0x0F > 0x09) {
            correction |= 0x6;
        }

        if carry || (!sub && accum > 0x99) {
            correction |= 0x60;
            set_carry = true;
        }

        accum = if sub {
            accum.wrapping_sub(correction)
        } else {
            accum.wrapping_add(correction)
        };

        self.set_reg_8(Reg8::A, accum);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, set_carry);
        self.set_flag(Flag::Zero, accum == 0);
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
            self.cycles += inst::CONDITIONAL_CYCLES[self.current_opcode as usize];
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
        let addr = self.reg_sp.get();
        self.write_mem_16(addr, val);
    }

    fn pop_stack(&mut self) -> u16 {
        let addr = self.reg_sp.get();
        self.reg_sp.inc(2);
        self.read_mem_16(addr)
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
            Operand8::MemImm(loc) => self.read_mem(loc),
            Operand8::MemReg(reg) => self.read_mem(self.get_reg_16(reg)),
            Operand8::MemHighImm(offset) => self.read_mem(0xFF00 | offset as u16),
            Operand8::MemHighC => {
                let offset = self.get_reg_8(Reg8::C);
                self.read_mem(0xFF00 | offset as u16)
            }
            Operand8::MemHlPostInc => {
                let hl = self.get_reg_16(Reg16::HL);
                let val = self.read_mem(hl);
                self.set_reg_16(Reg16::HL, hl.wrapping_add(1));
                val
            }
            Operand8::MemHlPostDec => {
                let hl = self.get_reg_16(Reg16::HL);
                let val = self.read_mem(hl);
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
            Operand8::MemImm(loc) => self.write_mem(loc, val),
            Operand8::MemReg(reg) => {
                let addr = self.get_reg_16(reg);
                self.write_mem(addr, val);
            }
            Operand8::MemHighImm(offset) => self.write_mem(0xFF00 | offset as u16, val),
            Operand8::MemHighC => {
                let offset = self.get_reg_8(Reg8::C);
                self.write_mem(0xFF00 | offset as u16, val);
            }
            Operand8::MemHlPostInc => {
                let hl = self.get_reg_16(Reg16::HL);
                self.write_mem(hl, val);
                self.set_reg_16(Reg16::HL, hl.wrapping_add(1));
            }
            Operand8::MemHlPostDec => {
                let hl = self.get_reg_16(Reg16::HL);
                self.write_mem(hl, val);
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
            Operand16::MemImm16(addr) => self.write_mem_16(addr, val),
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
            // The four low bits of F, the flag register, must always be zero.
            Reg16::AF => self.reg_af.set(val & 0xFFF0),
            Reg16::BC => self.reg_bc.set(val),
            Reg16::DE => self.reg_de.set(val),
            Reg16::HL => self.reg_hl.set(val),
            Reg16::SP => self.reg_sp.set(val),
            Reg16::PC => self.reg_pc.set(val),
        }
    }

    fn get_reg_16(&self, reg: Reg16) -> u16 {
        match reg {
            // The four low bits of F, the flag register, must always be zero.
            Reg16::AF => self.reg_af.get() & 0xFFF0,
            Reg16::BC => self.reg_bc.get(),
            Reg16::DE => self.reg_de.get(),
            Reg16::HL => self.reg_hl.get(),
            Reg16::SP => self.reg_sp.get(),
            Reg16::PC => self.reg_pc.get(),
        }
    }

    fn read_mem(&self, addr: u16) -> u8 {
        match addr {
            // First 16KB is ROM Bank 00 (in cartridge, fixed at bank 00)
            // Second 16KB are ROM Banks 01..NN (in cartridge, switchable bank number)
            0x0000...0x7FFF => self.cart.read(addr),

            // 8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
            0x8000...0x9FFF => {
                let i = (addr - 0x8000) as usize;
                self.gpu.read_vram(i)
            }

            // 8KB External RAM (in cartridge, switchable bank, if any)
            0xA000...0xBFFF => self.cart.read(addr),

            // C000-CFFF: 4KB Work RAM Bank 0 (WRAM)
            // D000-DFFF: 4KB Work RAM Bank 1 (WRAM) (switchable bank 1-7 in CGB Mode)
            //
            // NOTE: Since we don't support CGB mode yet, there is no switching and we handle this
            // like a contiguous 8KB block.
            0xC000...0xDFFF => {
                let i = (addr - 0xC000) as usize;
                self.work_ram[i]
            }

            // Same as C000-DDFF (ECHO) (typically not used)
            0xE000...0xFDFF => self.read_mem(addr - 0xE000 + 0xC000),

            // Sprite Attribute Table (OAM)
            0xFE00...0xFE9F => {
                let i = (addr - 0xFE00) as usize;
                self.gpu.read_sprite_ram(i)
            }

            // Not Usable
            //
            // This part of the address space is not connected to any hardware, but some games do
            // reads here. The result is 0xFF, the default value for the Gameboy data bus.
            0xFEA0...0xFEFF => 0xFF,

            // I/O Ports
            0xFF00...0xFF7F => self.read_io_port(addr as u8),

            // High RAM (HRAM)
            0xFF80...0xFFFE => {
                let i = (addr - 0xFF80) as usize;
                self.high_ram[i]
            }

            // Interrupt Enable Register
            0xFFFF => self.interrupt_enable_register,

            // This match is exhaustive but rustc doesn't check that for integer matches.
            _ => unreachable!(),
        }
    }

    fn write_mem(&mut self, addr: u16, val: u8) {
        trace!("mem[0x{:04X}] = 0x{:02X}", addr, val);

        match addr {
            // 32KB cartridge write
            0x0000...0x7FFF => self.cart.write(addr, val),

            // 8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
            0x8000...0x9FFF => {
                let i = (addr - 0x8000) as usize;
                self.gpu.write_vram(i, val);
            }

            // 8KB External RAM (in cartridge, switchable bank, if any)
            0xA000...0xBFFF => self.cart.write(addr, val),

            // C000-CFFF: 4KB Work RAM Bank 0 (WRAM)
            // D000-DFFF: 4KB Work RAM Bank 1 (WRAM) (switchable bank 1-7 in CGB Mode)
            //
            // NOTE: Since we don't support CGB mode yet, there is no switching and we handle this
            // like a contiguous 8KB block.
            0xC000...0xDFFF => {
                let i = (addr - 0xC000) as usize;
                self.work_ram[i] = val;
            }

            // Same as C000-DDFF (ECHO) (typically not used)
            0xE000...0xFDFF => self.write_mem(addr - 0xE000 + 0xC000, val),

            // Sprite Attribute Table (OAM). TODO: Can only write during H-Blank or V-Blank phase
            0xFE00...0xFE9F => {
                let i = (addr - 0xFE00) as usize;
                self.gpu.write_sprite_ram(i, val);
            }

            // Not Usable
            //
            // This part of the address space is not connected to any hardware, but some games do
            // writes here (I'm looking at you, Tetris). They are to be silently ignored.
            0xFEA0...0xFEFF => {}

            // I/O Ports
            0xFF00...0xFF7F => self.write_io_port(addr as u8, val),

            // High RAM (HRAM)
            0xFF80...0xFFFE => {
                let i = (addr - 0xFF80) as usize;
                self.high_ram[i] = val;
            }

            // Interrupt Enable Register
            0xFFFF => self.interrupt_enable_register = val,

            // This match is exhaustive but rustc doesn't check that for integer matches.
            _ => unreachable!(),
        }
    }

    fn read_mem_16(&mut self, addr: u16) -> u16 {
        let low = self.read_mem(addr);
        let high = self.read_mem(addr.wrapping_add(1));
        ((high as u16) << 8) | low as u16
    }

    fn write_mem_16(&mut self, addr: u16, val: u16) {
        self.write_mem(addr, val as u8);
        self.write_mem(addr.wrapping_add(1), (val >> 8) as u8);
    }

    fn read_io_port(&self, port: u8) -> u8 {
        let warn_unimplemented = |desc| {
            warn!("unimplemented: read from {} I/O port FF{:02X}; returning 0xFF", desc, port);
            0xFF
        };

        match port {
            // P1/JOYP - Joypad
            0x00 => self.joypad.read_reg(),

            0x01 | 0x02 => warn_unimplemented("serial"),

            // Unmapped
            0x03 => 0xFF,

            0x04...0x07 => self.timer.read_mem(port),

            // Unmapped
            0x08...0x0E => 0xFF,

            // IF - Interrupt Flag register
            // The top 3 bits are unused and always 1.
            0x0F => 0b1110_0000 | self.interrupt_flags_register,

            0x10...0x14 => warn_unimplemented("sound"),

            // Unmapped
            0x15 => 0xFF,

            0x16...0x1E => warn_unimplemented("sound"),

            // Unmapped
            0x1F => 0xFF,

            0x20...0x26 => warn_unimplemented("sound"),

            // Unmapped
            0x27...0x2F => 0xFF,

            0x30...0x3F => warn_unimplemented("sound"),

            // LCD Control Register
            0x40 => self.gpu.read_lcd_control(),
            0x41 => self.gpu.read_lcd_stat(),
            0x42 => self.gpu.scan_y,
            0x43 => self.gpu.scan_x,
            0x44 => self.gpu.scan_line,
            0x45 => self.gpu.scan_line_compare,
            0x46 => 0, // Cannot read from DMA transfer register
            0x47 => self.gpu.background_palette,
            0x48 => self.gpu.obj_palette_0,
            0x49 => self.gpu.obj_palette_1,
            0x4A => self.gpu.window_y,
            0x4B => self.gpu.window_x.wrapping_add(7),

            // Unmapped
            0x4C...0x7F => 0xFF,

            _ => panic!("unimplemented: read from I/O port FF{:02X}", port),
        }
    }

    fn write_io_port(&mut self, port: u8, val: u8) {
        let warn_unimplemented = |desc| {
            warn!(
                "unimplemented: write to {} I/O port FF{:02X}: 0x{2:02X} / 0b{2:08b} / {2}",
                desc, port, val
            );
        };

        match port {
            0x00 => self.joypad.write_reg(val),

            0x01 | 0x02 => warn_unimplemented("serial"),

            // Unmapped
            0x03 => {}

            0x04...0x07 => self.timer.write_mem(port, val),

            // Unmapped
            0x08...0x0E => {}

            // IF - Interrupt Flag register
            0x0F => self.interrupt_flags_register = val,

            0x10...0x14 => warn_unimplemented("sound"),

            // Unmapped
            0x15 => {}

            0x16...0x1E => warn_unimplemented("sound"),

            // Unmapped
            0x1F => {},

            0x20...0x26 => warn_unimplemented("sound"),

            // Unmapped
            0x27...0x2F => {},

            0x30...0x3F => warn_unimplemented("sound"),

            0x40 => self.gpu.write_lcd_control(val),
            0x41 => self.gpu.write_lcd_stat(val),
            0x42 => self.gpu.scan_y = val,
            0x43 => self.gpu.scan_x = val,
            0x44 => self.gpu.scan_line = 0,
            0x45 => self.gpu.scan_line_compare = val,
            // DMA Transfer - Takes 160 microseconds to complete. During this time, only HRAM can
            // be accessed.
            0x46 => {
                info!("DMA TRANSFER START");
                let start_addr: u16 = val as u16 * 0x100; // Addresses are from 0xXX00 - 0xXX9F
                for i in 0..0xA0 {
                    self.write_mem(0xFE00 + i, self.read_mem(start_addr + i))
                }
            }
            0x47 => self.gpu.background_palette = val,
            0x48 => self.gpu.obj_palette_0 = val,
            0x49 => self.gpu.obj_palette_1 = val,
            0x4A => self.gpu.window_y = val,
            0x4B => self.gpu.window_x = val.wrapping_sub(7),

            // Unmapped
            0x4C...0x7F => {},

            _ => panic!("unimplemented: write to I/O port FF{:02X}", port),
        }
    }
}

/// Returns true if `left + right` should set the half-carry flag, i.e. it requires a carry
/// from bit 3 into bit 4.
fn get_add_half_carry(left: u8, right: u8) -> bool {
    (left & 0xF) + (right & 0xF) > 0xF
}

/// Returns true if `left - right` should set the half-carry flag, i.e. it requires a borrow
/// from bit 4 into bit 3.
fn get_sub_half_carry(left: u8, right: u8) -> bool {
    (left & 0xF) < (right & 0xF)
}

/// Returns true if `left + right` should set the half-carry flag, i.e. it requires a carry
/// from bit 11 into bit 12.
///
/// This is for 16-bit adds, where the half-carry is set based on the halfway point of the high
/// byte.
fn get_add_half_carry_high(left: u16, right: u16) -> bool {
    (left & 0xFFF) + (right & 0xFFF) > 0xFFF
}

#[cfg(test)]
mod tests {
    use quickcheck::{quickcheck, TestResult};
    use std::fmt::{Debug, UpperHex, Write};
    use std::mem;
    use super::*;

    fn setup(rom: Vec<u8>) -> (Cpu, Cpu) {
        use crate::cart::{Cart, CartConfig};
        use crate::cart_header::MbcType;
        let rom_size = rom.len();
        let cart_config = CartConfig { mbc_type: MbcType::NoMbc, rom_size, ram_size: 0 };
        let mut actual = Cpu::new(Cart::new(rom.into_boxed_slice(), None, &cart_config).unwrap());
        let mut expected = actual.clone();
        actual.reg_pc.set(0);
        expected.reg_pc.set(rom_size as u16);
        (actual, expected)
    }

    /// Check if the actual and expected results are the same, pretty-printing any differences, and
    /// panicking (failing the test) if there are any differences.
    fn check_diff(actual: &Cpu, expected: &Cpu) -> TestResult {
        let mut err = String::new();

        diff_hex("AF register", &actual.reg_af.get(), &expected.reg_af.get(), &mut err);
        diff_hex("BC register", &actual.reg_bc.get(), &expected.reg_bc.get(), &mut err);
        diff_hex("DE register", &actual.reg_de.get(), &expected.reg_de.get(), &mut err);
        diff_hex("HL register", &actual.reg_hl.get(), &expected.reg_hl.get(), &mut err);
        diff_hex("SP register", &actual.reg_sp.get(), &expected.reg_sp.get(), &mut err);
        diff_hex("PC register", &actual.reg_pc.get(), &expected.reg_pc.get(), &mut err);
        diff(
            "interrupts_enabled",
            &actual.interrupts_enabled,
            &expected.interrupts_enabled,
            &mut err,
        );
        diff(
            "pending_disable_interrupts",
            &actual.pending_disable_interrupts,
            &expected.pending_disable_interrupts,
            &mut err,
        );
        diff(
            "pending_enable_interrupts",
            &actual.pending_enable_interrupts,
            &expected.pending_enable_interrupts,
            &mut err,
        );

        let actual_work_ram = actual.work_ram.iter();
        let expected_work_ram = expected.work_ram.iter();
        for (i, (actual_cell, expected_cell)) in actual_work_ram.zip(expected_work_ram).enumerate() {
            let name = format!("work_ramory location 0x{:02X}", i);
            diff_hex(&name, actual_cell, expected_cell, &mut err);
        }

        let actual_rom = actual.cart.rom().iter();
        let expected_rom = expected.cart.rom().iter();
        for (i, (actual_cell, expected_cell)) in actual_rom.zip(expected_rom).enumerate() {
            let name = format!("ROM location 0x{:02X}", i);
            diff_hex(&name, actual_cell, expected_cell, &mut err);
        }

        if err.is_empty() {
            TestResult::passed()
        } else {
            TestResult::error(err)
        }
    }

    /// Returns whether the actual and expected numbers are the same. Pretty-prints the numbers in
    /// hex if they differ.
    fn diff_hex<T: Debug + Eq + UpperHex>(name: &str, actual: &T, expected: &T, err: &mut String) {
        if actual != expected {
            let width = mem::size_of::<T>() * 2; // Number of hex digits for type T.
            writeln!(err, "\ndifference in {}:", name).unwrap();
            writeln!(err, "  actual:   0x{0:01$X} ({0:?})", actual, width).unwrap();
            writeln!(err, "  expected: 0x{0:01$X} ({0:?})", expected, width).unwrap();
        }
    }

    /// Returns whether the actual and expected values are the same. Pretty-prints the values if
    /// they differ.
    fn diff<T: Debug + Eq>(name: &str, actual: &T, expected: &T, err: &mut String) {
        if actual != expected {
            writeln!(err, "\ndifference in {}:", name).unwrap();
            writeln!(err, "  actual:   {:?}", actual).unwrap();
            writeln!(err, "  expected: {:?}", expected).unwrap();
        }
    }

    /// A helper for the `cpu_tests!` macro. This handles the `setup` and `expect` sections, which
    /// set fields on the initial and expected `Cpu`s, respectively.
    macro_rules! setup_cpu {
        (
            $cpu:ident,
            {
                $( reg8  { $( $reg8:ident  = $reg8val:expr  ),* $(,)* } )*
                $( reg16 { $( $reg16:ident = $reg16val:expr ),* $(,)* } )*
            }
        ) => ({
            $( $( $cpu.set_reg_8(Reg8::$reg8, $reg8val); )* )*
            $( $( $cpu.set_reg_16(Reg16::$reg16, $reg16val); )* )*
        })
    }

    /// A macro for writing concise machine code CPU tests.
    ///
    /// # Format
    ///
    /// ```
    /// test_name(var1: var1_ty, var2: var2_ty, ...) {
    ///     rom = [0x00, 0x01, ...],
    ///     setup {
    ///         reg8 {
    ///             A = 0,
    ///             B = 1,
    ///             ...
    ///         }
    ///         reg16 {
    ///             AF = 0,
    ///             DE = 1,
    ///             ...
    ///         }
    ///     }
    ///     expect {
    ///         reg8 {
    ///             A = 0,
    ///             B = 1,
    ///             ...
    ///         }
    ///         reg16 {
    ///             AF = 0,
    ///             DE = 1,
    ///             ...
    ///         }
    ///     }
    /// }
    /// ```
    ///
    /// The arguments (`var1`, `var2`, ...) are randomly generated values of their repective types
    /// from quickcheck. The argument types must each implement quickcheck's `Arbitrary` trait.
    /// Many primitive and standard library types already do.
    ///
    /// The `setup` and `expect` sections are optional, as are their `reg8` and `reg16`
    /// subsections. The `reg8` and `reg16` sections must use identifiers matching the `Reg8` and
    /// `Reg16` enum variants, respectively.
    macro_rules! cpu_tests {
        (
            $(
                $name:ident(
                    $($var:ident : $var_ty:ty),* $(,)*
                ) {
                    rom = [ $( $rom:expr ),* $(,)* ] $(,)*
                    $(setup $setup:tt)*
                    $(expect $expect:tt)*
                }
            )*
        ) => (
            $(
                quickcheck! {
                    #[allow(unused_mut)]
                    fn $name(
                        $($var : $var_ty),*
                    ) -> TestResult {
                        let rom = vec![ $( $rom ),* ];
                        let rom_size = rom.len();
                        let (mut actual, mut expected) = setup(rom);
                        $( setup_cpu!(actual, $setup); )*
                        $( setup_cpu!(expected, $expect); )*
                        while actual.reg_pc.get() as usize != rom_size {
                            actual.step();
                        }
                        check_diff(&actual, &expected)
                    }
                }
            )*
        );
    }

    // Helper functions for putting low/high bytes of 16-bit values into test ROMs.

    fn low(x: u16) -> u8 {
        (x & 0xFF) as u8
    }

    fn high(x: u16) -> u8 {
        ((x >> 8) & 0xFF) as u8
    }

    /// The actual tests.
    cpu_tests! {
        test_nop() {
            rom = [0x00], // nop
        }

        test_ld_reg8_imm8(a: u8, b: u8, c: u8, d: u8, e: u8, h: u8, l: u8) {
            rom = [
                0x3E, a, // ld a, $a
                0x06, b, // ld b, $b
                0x0E, c, // ld c, $c
                0x16, d, // ld d, $d
                0x1E, e, // ld e, $e
                0x26, h, // ld h, $h
                0x2E, l, // ld l, $l
            ],
            setup  { reg8 { A = 0, B = 0, C = 0, D = 0, E = 0, H = 0, L = 0 } }
            expect { reg8 { A = a, B = b, C = c, D = d, E = e, H = h, L = l } }
        }

        test_ld_reg16_imm16(bc: u16, de: u16, hl: u16, sp: u16) {
            rom = [
                0x01, low(bc), high(bc), // ld bc, $bc
                0x11, low(de), high(de), // ld de, $de
                0x21, low(hl), high(hl), // ld hl, $hl
                0x31, low(sp), high(sp), // ld sp, $sp
            ],
            setup  { reg16 { BC = 0,  DE = 0,  HL = 0,  SP = 0 } }
            expect { reg16 { BC = bc, DE = de, HL = hl, SP = sp } }
        }

        test_jp_imm16_unconditional() {
            // Test that we can jump past a halt instruction without stopping.
            rom = [
                0xC3, 0x04, 0x00, // jp 0x0004
                0x76,             // halt
            ],
        }
    }
}
