use crate::audio::Audio;
use crate::cart::Cart;
use crate::gpu::Gpu;
use crate::interrupts::Interrupt;
use crate::joypad::Joypad;
use crate::timer::Timer;
use enumflags2::BitFlags;
use log::{debug, info, log_enabled, trace, warn};
use self::inst::{Cond, Inst, Operand16, Operand8};
use self::registers::{Flag, Reg16, Reg8, Registers};

mod inst;
mod registers;

#[cfg(test)]
mod test;

// TODO: Refactor later to extract memory-related stuff out of the cpu module.
const WORK_RAM_SIZE: usize = 8 * 1024; // 8 KB
const HIGH_RAM_SIZE: usize = 127; // For the address range 0xFF80-0xFFFE (inclusive).

#[derive(Clone)]
pub struct Cpu {
    /// The core CPU registers.
    regs: Registers,

    /// Work RAM internal to the Game Boy, as opposed to external cartridge RAM. Limited to 8 KB in
    /// the original Game Boy.
    work_ram: Box<[u8]>,

    /// High RAM internal to the Game Boy. This is a small range of 127 bytes at 0xFF80-0xFFFE.
    high_ram: Box<[u8]>,

    /// The Game Boy timing registers
    timer: Timer,

    /// The graphics procession unit.
    pub gpu: Gpu,

    /// The player controller hardware.
    pub joypad: Joypad,

    /// Game cartridge.
    pub cart: Cart,

    /// The audio processing unit.
    pub audio: Audio,

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
    interrupt_flags_register: BitFlags<Interrupt>,

    /// The `IE` Interrupt Enable register accessed via I/O port 0xFFFF.
    interrupt_enable_register: BitFlags<Interrupt>,

    /// Contains the 3 unused bits of the IE register, which nonetheless are read/write-able on the
    /// Game Boy.
    // TODO(solson): Refactor to combine this with the `interrupt_enable_register` field.
    interrupt_enable_unused_bits: u8,

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
        Cpu {
            regs: Registers::new(),
            work_ram: vec![0; WORK_RAM_SIZE].into_boxed_slice(),
            high_ram: vec![0; HIGH_RAM_SIZE].into_boxed_slice(),
            timer: Timer::new(),
            gpu: Gpu::new(),
            joypad: Joypad::new(),
            audio: Audio::new(),
            cart,
            current_opcode: 0,
            cycles: 0,
            interrupts_enabled: false,
            pending_disable_interrupts: false,
            pending_enable_interrupts: false,
            interrupt_flags_register: BitFlags::from(Interrupt::VBlank),
            interrupt_enable_register: BitFlags::empty(),
            interrupt_enable_unused_bits: 0,
            halted: false,
            stopped: false,
            debug_symbols: None,
        }
    }

    /// Keep executing instructions until more than the given number of cycles have passed.
    pub fn step_cycles(&mut self, cycles: usize) {
        let mut curr_cycles: usize = 0;
        while curr_cycles < cycles {
            let mut interrupts = BitFlags::empty();
            let step_cycles = self.step();
            interrupts |= self.gpu.step(step_cycles);
            interrupts |= self.timer.step(step_cycles);
            interrupts |= self.joypad.step();
            self.request_interrupts(interrupts);
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
        let base_pc = self.regs.pc.get();
        self.current_opcode = self.read_mem(base_pc);
        let instruction_len = inst::INSTRUCTION_LENGTH[self.current_opcode as usize];
        self.regs.pc += instruction_len as u16;

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
        use Interrupt::*;
        for &i in &[VBlank, Lcd, Timer, Serial, Joypad] {
            self.check_interrupt(i);
        }
        if self.interrupt_flags_register.contains(Joypad) {
            self.stopped = false;
        }
    }

    fn check_interrupt(&mut self, i: Interrupt) {
        let flagged = self.interrupt_flags_register.contains(i);
        let enabled = self.interrupt_enable_register.contains(i);
        if flagged { self.halted = false; }
        if self.interrupts_enabled && enabled && flagged {
            debug!("Handling interrupt {:?}", i);
            // TODO(solson): Use `call` or `call_restart`?
            self.push_stack(self.regs.pc.get());
            self.regs.pc.set(i.handler_addr());
            self.interrupt_flags_register.remove(i);
        }
    }

    pub fn request_interrupts(&mut self, interrupts: BitFlags<Interrupt>) {
        if log_enabled!(log::Level::Debug) {
            for i in interrupts.iter() {
                debug!("Requesting interrupt {:?}", i);
            }
        }
        self.interrupt_flags_register |= interrupts;
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
            Inst::Rlc(n) => self.rotate_left_circular(n, true),
            Inst::Rl(n) => self.rotate_left(n, true),
            Inst::Rrc(n) => self.rotate_right_circular(n, true),
            Inst::Rr(n) => self.rotate_right(n, true),
            Inst::Rlca => self.rotate_left_circular(Operand8::Reg8(Reg8::A), false),
            Inst::Rla => self.rotate_left(Operand8::Reg8(Reg8::A), false),
            Inst::Rrca => self.rotate_right_circular(Operand8::Reg8(Reg8::A), false),
            Inst::Rra => self.rotate_right(Operand8::Reg8(Reg8::A), false),
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

            self.regs.pc.set(addr_val);
        }
    }

    /// The `Inst::Jr` instruction.
    ///
    /// Increment the program counter by the given offset if the condition is met.
    fn jump_relative(&mut self, offset: i8, cond: Cond) {
        if self.check_cond_and_update_cycles(cond) {
            self.regs.pc += offset;
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
            self.push_stack(self.regs.pc.get());
            self.regs.pc.set(fn_addr);
        }
    }

    /// The `Inst::Rst` instruction.
    ///
    /// Call the "restart" function at the given address.
    fn call_restart(&mut self, addr: u8) {
        // TODO(solson): Use `call` function?
        self.push_stack(self.regs.pc.get());
        self.regs.pc.set(addr as u16);
    }

    /// The `Inst::Ret` instruction.
    fn ret(&mut self, cond: Cond) {
        if self.check_cond_and_update_cycles(cond) {
            let return_addr = self.pop_stack();
            self.regs.pc.set(return_addr);
        }
    }

    /// The `Inst::Push` instruction.
    fn push(&mut self, reg: Reg16) {
        self.push_stack(self.regs.get_16(reg));
    }

    /// The `Inst::Pop` instruction.
    fn pop(&mut self, reg: Reg16) {
        let val = self.pop_stack();
        self.regs.set_16(reg, val);
    }

    /// The 8-bit `Inst::Ld` instruction.
    ///
    /// Move 8 bits from `src` and store them into `dest`.
    fn move_8(&mut self, dest: Operand8, src: Operand8) {
        match (dest, src) {
            (Operand8::Reg8(Reg8::B), Operand8::Reg8(Reg8::B)) => panic!("debug opcode"),
            _ => {}
        }
        let val = self.get_operand_8(src);
        self.set_operand_8(dest, val);
    }

    /// The 16-bit `Inst::Ld` instruction.
    ///
    /// Move 16 bits from `src` and store them into `dest`.
    fn move_16(&mut self, dest: Operand16, src: Operand16) {
        self.set_operand_16(dest, self.get_operand_16(src));
    }

    /// The `Inst::LdHlSp` and `Inst::AddSp` instructions.
    ///
    /// Loads the stack pointer plus an 8-bit signed value into the specified register.
    fn load_stack_addr_into_reg(&mut self, offset: i8, dest: Reg16) {
        let sp = self.regs.sp.get();
        let val = (sp as i32 + offset as i32) as u16;
        self.regs.set_16(dest, val);
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
        let accum = self.regs.a;
        let n_val = self.get_operand_8(n);
        let (new_accum, carry) = accum.overflowing_add(n_val);
        self.regs.a = new_accum;
        self.set_flag(Flag::Zero, new_accum == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, get_add_half_carry(accum, n_val));
        self.set_flag(Flag::Carry, carry);
    }

    /// The `Inst::AddHl` instruction
    fn add_hl(&mut self, n: Operand16) {
        let old_hl = self.regs.hl.get();
        let n_val = self.get_operand_16(n);
        let (new_hl, carry) = old_hl.overflowing_add(n_val);
        self.regs.hl.set(new_hl);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, get_add_half_carry_high(old_hl, n_val));
        self.set_flag(Flag::Carry, carry);
    }

    /// The `Inst::AdcA` instruction
    fn add_accum_with_carry(&mut self, n: Operand8) {
        let accum = self.regs.a;
        let n_val = self.get_operand_8(n);
        let carry_val = self.get_flag(Flag::Carry) as u8;
        let (midway_accum, midway_carry) = accum.overflowing_add(n_val);
        let (final_accum, final_carry) = midway_accum.overflowing_add(carry_val);
        self.regs.a = final_accum;
        self.set_flag(Flag::Zero, final_accum == 0);
        self.set_flag(Flag::Sub, false);
        let half_carry = get_add_half_carry(accum, n_val) ||
            get_add_half_carry(midway_accum, carry_val);
        self.set_flag(Flag::HalfCarry, half_carry);
        self.set_flag(Flag::Carry, midway_carry || final_carry);
    }

    /// The `Inst::Sub` instruction
    fn sub_accum(&mut self, n: Operand8) {
        let accum = self.regs.a;
        let n_val = self.get_operand_8(n);
        let (new_accum, carry) = accum.overflowing_sub(n_val);
        self.regs.a = new_accum;
        self.set_flag(Flag::Zero, new_accum == 0);
        self.set_flag(Flag::Sub, true);
        self.set_flag(Flag::HalfCarry, get_sub_half_carry(accum, n_val));
        self.set_flag(Flag::Carry, carry);
    }

    /// The `Inst::SbcA` instruction
    fn sub_accum_with_carry(&mut self, n: Operand8) {
        let accum = self.regs.a;
        let n_val = self.get_operand_8(n);
        let carry_val = self.get_flag(Flag::Carry) as u8;
        let (midway_accum, midway_carry) = accum.overflowing_sub(n_val);
        let (final_accum, final_carry) = midway_accum.overflowing_sub(carry_val);
        self.regs.a = final_accum;
        self.set_flag(Flag::Zero, final_accum == 0);
        self.set_flag(Flag::Sub, true);
        let half_carry = get_sub_half_carry(accum, n_val) ||
            get_sub_half_carry(midway_accum, carry_val);
        self.set_flag(Flag::HalfCarry, half_carry);
        self.set_flag(Flag::Carry, midway_carry || final_carry);
    }

    /// The `Inst::And` instruction.
    fn and_accum(&mut self, n: Operand8) {
        self.regs.a &= self.get_operand_8(n);
        self.set_flag(Flag::Zero, self.regs.a == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, true);
        self.set_flag(Flag::Carry, false);
    }

    /// The `Inst::Xor` instruction.
    fn xor_accum(&mut self, n: Operand8) {
        self.regs.a ^= self.get_operand_8(n);
        self.set_flag(Flag::Zero, self.regs.a == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, false);
    }

    /// The `Inst::Or` instruction.
    fn or_accum(&mut self, n: Operand8) {
        self.regs.a |= self.get_operand_8(n);
        self.set_flag(Flag::Zero, self.regs.a == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, false);
    }

    /// The `Inst::Cp` instruction.
    fn compare_accum(&mut self, n: Operand8) {
        let left = self.regs.a;
        let right = self.get_operand_8(n);
        self.set_flag(Flag::Zero, left == right);
        self.set_flag(Flag::Sub, true);
        self.set_flag(Flag::HalfCarry, get_sub_half_carry(left, right));
        self.set_flag(Flag::Carry, left < right);
    }

    /// The `Inst::Rlc` instruction.
    fn rotate_left_circular(&mut self, n: Operand8, set_zero_flag: bool) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val.rotate_left(1);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, set_zero_flag && new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x80 != 0);
    }

    /// The `Inst::Rl` instruction.
    fn rotate_left(&mut self, n: Operand8, set_zero_flag: bool) {
        let old_val = self.get_operand_8(n);
        let old_carry = self.get_flag(Flag::Carry) as u8;
        let new_val = (old_val << 1) | old_carry;
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, set_zero_flag && new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x80 != 0);
    }

    /// The `Inst::Rrc` instruction.
    fn rotate_right_circular(&mut self, n: Operand8, set_zero_flag: bool) {
        let old_val = self.get_operand_8(n);
        let new_val = old_val.rotate_right(1);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, set_zero_flag && new_val == 0);
        self.set_flag(Flag::Sub, false);
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, old_val & 0x01 != 0);
    }

    /// The `Inst::Rr` instruction.
    fn rotate_right(&mut self, n: Operand8, set_zero_flag: bool) {
        let old_val = self.get_operand_8(n);
        let old_carry = self.get_flag(Flag::Carry) as u8;
        let new_val = (old_carry << 7) | (old_val >> 1);
        self.set_operand_8(n, new_val);
        self.set_flag(Flag::Zero, set_zero_flag && new_val == 0);
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

        let mut accum = self.regs.a;
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

        self.regs.a = accum;
        self.set_flag(Flag::HalfCarry, false);
        self.set_flag(Flag::Carry, set_carry);
        self.set_flag(Flag::Zero, accum == 0);
    }

    /// The `Inst::Cpl` instruction.
    fn complement_accum(&mut self) {
        self.regs.a = !self.regs.a;
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
        self.regs.sp -= 2u16;
        self.write_mem_16(self.regs.sp.get(), val);
    }

    fn pop_stack(&mut self) -> u16 {
        let addr = self.regs.sp.get();
        self.regs.sp += 2u16;
        self.read_mem_16(addr)
    }

    fn set_flag(&mut self, flag: Flag, val: bool) {
        if val {
            self.regs.f.insert(flag);
        } else {
            self.regs.f.remove(flag);
        }
    }

    fn get_flag(&self, flag: Flag) -> bool {
        self.regs.f.contains(flag)
    }

    /// Get the value of the given 8-bit operand.
    ///
    /// NOTE: Accessing some operands has side effects, so you should not call this twice on a
    /// given operand.
    fn get_operand_8(&mut self, src: Operand8) -> u8 {
        match src {
            Operand8::Imm8(val) => val,
            Operand8::Reg8(reg) => self.regs.get_8(reg),
            Operand8::MemImm(loc) => self.read_mem(loc),
            Operand8::MemReg(reg) => self.read_mem(self.regs.get_16(reg)),
            Operand8::MemHighImm(offset) => self.read_mem(0xFF00 | offset as u16),
            Operand8::MemHighC => self.read_mem(0xFF00 | self.regs.bc.low() as u16),
            Operand8::MemHlPostInc => {
                let val = self.read_mem(self.regs.hl.get());
                self.regs.hl += 1u16;
                val
            }
            Operand8::MemHlPostDec => {
                let val = self.read_mem(self.regs.hl.get());
                self.regs.hl -= 1u16;
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
            Operand8::Reg8(reg) => self.regs.set_8(reg, val),
            Operand8::MemImm(loc) => self.write_mem(loc, val),
            Operand8::MemReg(reg) => self.write_mem(self.regs.get_16(reg), val),
            Operand8::MemHighImm(offset) => self.write_mem(0xFF00 | offset as u16, val),
            Operand8::MemHighC => self.write_mem(0xFF00 | self.regs.bc.low() as u16, val),
            Operand8::MemHlPostInc => {
                self.write_mem(self.regs.hl.get(), val);
                self.regs.hl += 1u16;
            }
            Operand8::MemHlPostDec => {
                self.write_mem(self.regs.hl.get(), val);
                self.regs.hl -= 1u16;
            }
        }
    }

    fn get_operand_16(&self, src: Operand16) -> u16 {
        match src {
            Operand16::Imm16(val) => val,
            Operand16::Reg16(reg) => self.regs.get_16(reg),
            Operand16::MemImm16(_) => panic!("no Game Boy CPU instruction actually uses this"),
        }
    }

    fn set_operand_16(&mut self, dest: Operand16, val: u16) {
        match dest {
            Operand16::Imm16(_) => panic!("Attempt to store to a 16-bit immediate value"),
            Operand16::Reg16(reg) => self.regs.set_16(reg, val),
            Operand16::MemImm16(addr) => self.write_mem_16(addr, val),
        }
    }

    fn read_mem(&self, addr: u16) -> u8 {
        let val = match addr {
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
            // reads here. The result is 0xFF, the default value for the Game Boy data bus.
            0xFEA0...0xFEFF => 0xFF,

            // I/O Ports
            0xFF00...0xFF7F => self.read_io_port(addr as u8),

            // High RAM (HRAM)
            0xFF80...0xFFFE => {
                let i = (addr - 0xFF80) as usize;
                self.high_ram[i]
            }

            // Interrupt Enable Register
            // The top 3 bits are unused and always 1.
            0xFFFF => {
                (self.interrupt_enable_unused_bits & 0b1110_0000) |
                    self.interrupt_enable_register.bits()
            }
        };

        trace!("read(0x{:04X}) => 0x{:02X}", addr, val);

        val
    }

    fn write_mem(&mut self, addr: u16, val: u8) {
        trace!("write(0x{:04X}, 0x{:02X})", addr, val);

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
            0xFFFF => {
                self.interrupt_enable_register = BitFlags::from_bits_truncate(val);
                self.interrupt_enable_unused_bits = val & 0b1110_0000;
            }
        }
    }

    fn read_mem_16(&mut self, addr: u16) -> u16 {
        let low = self.read_mem(addr);
        let high = self.read_mem(addr.wrapping_add(1));
        u16::from_le_bytes([low, high])
    }

    fn write_mem_16(&mut self, addr: u16, val: u16) {
        let [low, high] = val.to_le_bytes();
        self.write_mem(addr, low);
        self.write_mem(addr.wrapping_add(1), high);
    }

    fn read_io_port(&self, port: u8) -> u8 {
        let warn_unimplemented = |desc| {
            warn!("unimplemented: read from {} I/O port FF{:02X}; returning 0xFF", desc, port);
            0xFF
        };

        match port {
            0x00 => self.joypad.read_reg(),
            0x01...0x02 => warn_unimplemented("serial"),
            0x04...0x07 => self.timer.read_reg(port),
            // The top 3 bits are unused and always 1.
            0x0F => 0b1110_0000 | self.interrupt_flags_register.bits(),
            0x10...0x14 | 0x16...0x19 | 0x20...0x26 => warn_unimplemented("sound"),
            0x1A...0x1E => self.audio.read_reg(port),
            0x30...0x3F => self.audio.read_reg(port),
            0x40...0x45 | 0x47...0x4B => self.gpu.read_reg(port),

            // Cannot read from DMA transfer register.
            // TODO(solson): Should it return 0xFF like most other inaccessible registers?
            0x46 => 0,

            // Unmapped I/O ports always return all bits high.
            0x03 | 0x08...0x0E | 0x15 | 0x1F | 0x27...0x2F | 0x4C...0x7F => 0xFF,

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
            0x01...0x02 => warn_unimplemented("serial"),
            0x04...0x07 => self.timer.write_reg(port, val),
            0x0F => self.interrupt_flags_register = BitFlags::from_bits_truncate(val),
            0x10...0x14 | 0x16...0x19 | 0x20...0x26 => warn_unimplemented("sound"),
            0x1A...0x1E => self.audio.write_reg(port, val),
            0x30...0x3F => self.audio.write_reg(port, val),
            0x40...0x45 | 0x47...0x4B => self.gpu.write_reg(port, val),

            // DMA Transfer - Takes 160 microseconds to complete. During this time, only HRAM can
            // be accessed.
            // TODO(solson): Actually implement the HRAM memory restriction, and make OAM happen
            // over time rather than in a single instant.
            0x46 => {
                info!("DMA TRANSFER START");
                let start_addr: u16 = val as u16 * 0x100; // Addresses are from 0xXX00 - 0xXX9F
                for i in 0..0xA0 {
                    self.write_mem(0xFE00 + i, self.read_mem(start_addr + i))
                }
            }

            // Unmapped I/O ports always ignore writes.
            0x03 | 0x08...0x0E | 0x15 | 0x1F | 0x27...0x2F | 0x4C...0x7F => {}

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
