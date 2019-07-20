use quickcheck::{quickcheck, TestResult};
use std::fmt::{Debug, UpperHex, Write};
use std::mem;
use std::collections::HashSet;
use super::*;

fn setup(rom: Vec<u8>) -> (Cpu, Cpu) {
    use crate::cart::CartConfig;
    use crate::cart_header::CartType;
    let rom_size = rom.len();
    let cart_config = CartConfig { cart_type: CartType::NoMbc, rom_size, ram_size: 0 };
    let mut actual = Cpu::new(Cart::new(rom.into_boxed_slice(), None, &cart_config).unwrap());
    let mut expected = actual.clone();
    actual.regs.pc.set(0);
    expected.regs.pc.set(rom_size as u16);
    (actual, expected)
}

/// Check if the actual and expected results are the same, pretty-printing any differences, and
/// panicking (failing the test) if there are any differences.
fn check_diff(actual: &Cpu, expected: &Cpu) -> TestResult {
    let mut err = String::new();

    diff_hex("AF", &actual.regs.get_16(Reg16::AF), &expected.regs.get_16(Reg16::AF), &mut err);
    diff_hex("BC", &actual.regs.get_16(Reg16::BC), &expected.regs.get_16(Reg16::BC), &mut err);
    diff_hex("DE", &actual.regs.get_16(Reg16::DE), &expected.regs.get_16(Reg16::DE), &mut err);
    diff_hex("HL", &actual.regs.get_16(Reg16::HL), &expected.regs.get_16(Reg16::HL), &mut err);
    diff_hex("SP", &actual.regs.get_16(Reg16::SP), &expected.regs.get_16(Reg16::SP), &mut err);
    diff_hex("PC", &actual.regs.get_16(Reg16::PC), &expected.regs.get_16(Reg16::PC), &mut err);
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
        let name = format!("work_ram location 0x{:02X}", i);
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
        $( $( $cpu.regs.set_8(Reg8::$reg8, $reg8val); )* )*
        $( $( $cpu.regs.set_16(Reg16::$reg16, $reg16val); )* )*
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
                    while actual.regs.pc.get() as usize != rom_size {
                        actual.step(false, false, &HashSet::new());
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

// The actual tests.
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
