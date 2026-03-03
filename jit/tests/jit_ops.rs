//! Tests that JIT-compiled SM83 instructions produce correct register/memory/flag state.
//! Each test compiles a small block, runs it, and checks the EmulatorState.

use haskellboy_jit::engine::JITEngine;
use haskellboy_jit::state::{reg, flags, ExitReason};
use haskellboy_jit::state::state_offsets as off;

/// Helper: set up an EmulatorState backed by memory + registers arrays.
/// Returns (state_buf, memory, registers) — all must be kept alive during run_block.
struct TestState {
    state: Vec<u8>,
    memory: Vec<u8>,
    registers: Vec<u8>, // 8 bytes: A,F,B,C,D,E,H,L
}

impl TestState {
    fn new() -> Self {
        Self {
            state: vec![0u8; 64],
            memory: vec![0u8; 0x10000],
            registers: vec![0u8; 8],
        }
    }

    /// Write ROM bytes at the given address.
    fn write_rom(&mut self, addr: u16, bytes: &[u8]) {
        for (i, &b) in bytes.iter().enumerate() {
            self.memory[addr as usize + i] = b;
        }
    }

    /// Set a register value.
    fn set_reg(&mut self, r: u8, val: u8) {
        self.registers[r as usize] = val;
    }

    /// Set PC.
    fn set_pc(&mut self, pc: u16) {
        unsafe {
            std::ptr::copy_nonoverlapping(
                &pc as *const u16 as *const u8,
                self.state.as_mut_ptr().offset(off::PC as isize),
                2,
            );
        }
    }

    /// Set SP.
    fn set_sp(&mut self, sp: u16) {
        unsafe {
            std::ptr::copy_nonoverlapping(
                &sp as *const u16 as *const u8,
                self.state.as_mut_ptr().offset(off::SP as isize),
                2,
            );
        }
    }

    /// Sync pointers into the state buffer and return a pointer for run_block.
    fn prepare(&mut self) -> *mut u8 {
        unsafe {
            let mem_ptr = self.memory.as_mut_ptr();
            let reg_ptr = self.registers.as_mut_ptr();
            std::ptr::copy_nonoverlapping(
                &mem_ptr as *const _ as *const u8,
                self.state.as_mut_ptr().offset(off::MEMORY as isize),
                std::mem::size_of::<*mut u8>(),
            );
            std::ptr::copy_nonoverlapping(
                &reg_ptr as *const _ as *const u8,
                self.state.as_mut_ptr().offset(off::REGISTERS as isize),
                std::mem::size_of::<*mut u8>(),
            );
            // Zero cycles
            let zero: u16 = 0;
            std::ptr::copy_nonoverlapping(
                &zero as *const u16 as *const u8,
                self.state.as_mut_ptr().offset(off::CYCLES_USED as isize),
                2,
            );
            self.state.as_mut_ptr()
        }
    }

    /// Read back a register.
    fn get_reg(&self, r: u8) -> u8 {
        self.registers[r as usize]
    }

    /// Read back PC from state.
    fn get_pc(&self) -> u16 {
        unsafe {
            std::ptr::read_unaligned(
                self.state.as_ptr().offset(off::PC as isize) as *const u16
            )
        }
    }

    /// Read back SP from state.
    fn get_sp(&self) -> u16 {
        unsafe {
            std::ptr::read_unaligned(
                self.state.as_ptr().offset(off::SP as isize) as *const u16
            )
        }
    }

    /// Read back cycles used.
    fn get_cycles(&self) -> u16 {
        unsafe {
            std::ptr::read_unaligned(
                self.state.as_ptr().offset(off::CYCLES_USED as isize) as *const u16
            )
        }
    }

    /// Read back IO address (set on IoRead/IoWrite exit).
    fn get_io_addr(&self) -> u16 {
        unsafe {
            std::ptr::read_unaligned(
                self.state.as_ptr().offset(off::IO_ADDR as isize) as *const u16
            )
        }
    }

    /// Read a byte from memory.
    fn read_mem(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    /// Check if a flag is set in the F register.
    fn flag(&self, bit: u8) -> bool {
        (self.get_reg(reg::F) >> bit) & 1 == 1
    }
}

/// Compile a block at `pc` from the test state's memory and run it.
fn compile_and_run(engine: &mut JITEngine, ts: &mut TestState, pc: u16, downcount: u16) -> ExitReason {
    engine.compile_block(&ts.memory, pc).expect("compile failed");
    ts.set_pc(pc);
    let ptr = ts.prepare();
    engine.run_block(ptr, downcount)
}

// ============================================================
// FFI initialization tests
// ============================================================

#[test]
fn test_ffi_engine_lifecycle() {
    // Simulates the Haskell FFI call sequence: new → compile → run → free
    let mut engine = JITEngine::new();
    assert!(!engine.has_block(0x0100));

    let mut rom = vec![0u8; 0x10000];
    rom[0x0100] = 0x00; // NOP
    rom[0x0101] = 0xC3; // JP nn
    rom[0x0102] = 0x00;
    rom[0x0103] = 0x02; // JP 0x0200

    engine.compile_block(&rom, 0x0100).unwrap();
    assert!(engine.has_block(0x0100));

    engine.invalidate(0x0100);
    assert!(!engine.has_block(0x0100));
}

#[test]
fn test_ffi_c_api_round_trip() {
    // Test the actual C API functions (matching what Haskell calls)
    unsafe {
        let engine = haskellboy_jit::jit_engine_new();
        assert!(!engine.is_null(), "jit_engine_new returned null");

        let has = haskellboy_jit::jit_has_block(engine, 0x0100);
        assert_eq!(has, 0, "should not have block yet");

        let mut rom = vec![0u8; 0x10000];
        rom[0x0100] = 0x00; // NOP
        rom[0x0101] = 0xC3; // JP nn
        rom[0x0102] = 0x00;
        rom[0x0103] = 0x02;

        let rc = haskellboy_jit::jit_compile_block(engine, rom.as_ptr(), 0x0100, 0x10000);
        assert_eq!(rc, 0, "compile should succeed");

        let has = haskellboy_jit::jit_has_block(engine, 0x0100);
        assert_eq!(has, 1, "should have block after compile");

        haskellboy_jit::jit_invalidate(engine, 0x0100);
        let has = haskellboy_jit::jit_has_block(engine, 0x0100);
        assert_eq!(has, 0, "should not have block after invalidate");

        haskellboy_jit::jit_engine_free(engine);
    }
}

// ============================================================
// NOP and basic control flow
// ============================================================

#[test]
fn test_nop_jp() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // NOP; JP 0x0200
    ts.write_rom(0x0100, &[0x00, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    assert_eq!(ts.get_pc(), 0x0200);
    assert_eq!(ts.get_cycles(), 4 + 16); // NOP=4, JP=16
}

#[test]
fn test_jr_forward() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // JR +5  (at 0x0100, jumps to 0x0100 + 2 + 5 = 0x0107)
    ts.write_rom(0x0100, &[0x18, 0x05]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    assert_eq!(ts.get_pc(), 0x0107);
}

#[test]
fn test_jr_backward() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // JR -2  (at 0x0100, jumps to 0x0100 + 2 + (-2) = 0x0100)
    ts.write_rom(0x0100, &[0x18, 0xFE]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    assert_eq!(ts.get_pc(), 0x0100);
}

// ============================================================
// Register-to-register LD
// ============================================================

#[test]
fn test_ld_b_a() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x42);
    // LD B,A (0x47); JP 0x0200
    ts.write_rom(0x0100, &[0x47, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::B), 0x42);
    assert_eq!(ts.get_reg(reg::A), 0x42); // A unchanged
}

#[test]
fn test_ld_all_registers() {
    // LD A,0x11; LD B,A; LD C,A; LD D,A; LD E,A; LD H,A; LD L,A; JP 0x200
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.write_rom(0x0100, &[
        0x3E, 0x11, // LD A,0x11
        0x47,       // LD B,A
        0x4F,       // LD C,A
        0x57,       // LD D,A
        0x5F,       // LD E,A
        0x67,       // LD H,A
        0x6F,       // LD L,A
        0xC3, 0x00, 0x02, // JP 0x0200
    ]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x11);
    assert_eq!(ts.get_reg(reg::B), 0x11);
    assert_eq!(ts.get_reg(reg::C), 0x11);
    assert_eq!(ts.get_reg(reg::D), 0x11);
    assert_eq!(ts.get_reg(reg::E), 0x11);
    assert_eq!(ts.get_reg(reg::H), 0x11);
    assert_eq!(ts.get_reg(reg::L), 0x11);
}

// ============================================================
// ALU operations (ADD, SUB, AND, OR, XOR, CP)
// ============================================================

#[test]
fn test_add_a_b() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x3A);
    ts.set_reg(reg::B, 0xC6);
    // ADD A,B (0x80); JP 0x0200
    ts.write_rom(0x0100, &[0x80, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    // 0x3A + 0xC6 = 0x100 → A=0x00
    assert_eq!(ts.get_reg(reg::A), 0x00);
    assert!(ts.flag(flags::Z), "Z should be set (result is 0)");
    assert!(!ts.flag(flags::N), "N should be clear (addition)");
    assert!(ts.flag(flags::H), "H should be set (0xA + 0x6 = 0x10)");
    assert!(ts.flag(flags::C), "C should be set (overflow)");
}

#[test]
fn test_add_a_b_no_carry() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x10);
    ts.set_reg(reg::B, 0x20);
    ts.write_rom(0x0100, &[0x80, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x30);
    assert!(!ts.flag(flags::Z));
    assert!(!ts.flag(flags::N));
    assert!(!ts.flag(flags::H));
    assert!(!ts.flag(flags::C));
}

#[test]
fn test_sub_a_b() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x3E);
    ts.set_reg(reg::B, 0x3E);
    // SUB B (0x90); JP 0x0200
    ts.write_rom(0x0100, &[0x90, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x00);
    assert!(ts.flag(flags::Z), "Z set (result is 0)");
    assert!(ts.flag(flags::N), "N set (subtraction)");
    assert!(!ts.flag(flags::H), "H clear (no half-borrow)");
    assert!(!ts.flag(flags::C), "C clear (no borrow)");
}

#[test]
fn test_sub_a_b_borrow() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x10);
    ts.set_reg(reg::B, 0x20);
    // SUB B (0x90); JP 0x0200
    ts.write_rom(0x0100, &[0x90, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0xF0); // 0x10 - 0x20 = 0xF0
    assert!(!ts.flag(flags::Z));
    assert!(ts.flag(flags::N));
    assert!(!ts.flag(flags::H)); // 0x0 - 0x0 = no half-borrow
    assert!(ts.flag(flags::C), "C set (borrow)");
}

#[test]
fn test_and_a_b() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x5A);
    ts.set_reg(reg::B, 0x3F);
    // AND B (0xA0); JP 0x0200
    ts.write_rom(0x0100, &[0xA0, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x1A); // 0x5A & 0x3F = 0x1A
    assert!(!ts.flag(flags::Z));
    assert!(!ts.flag(flags::N));
    assert!(ts.flag(flags::H), "H always set for AND");
    assert!(!ts.flag(flags::C));
}

#[test]
fn test_or_a_b() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x5A);
    ts.set_reg(reg::B, 0x03);
    // OR B (0xB0); JP 0x0200
    ts.write_rom(0x0100, &[0xB0, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x5B); // 0x5A | 0x03 = 0x5B
    assert!(!ts.flag(flags::Z));
    assert!(!ts.flag(flags::N));
    assert!(!ts.flag(flags::H));
    assert!(!ts.flag(flags::C));
}

#[test]
fn test_xor_a_a() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0xFF);
    // XOR A (0xAF); JP 0x0200
    ts.write_rom(0x0100, &[0xAF, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x00); // A ^ A = 0
    assert!(ts.flag(flags::Z));
    assert!(!ts.flag(flags::N));
    assert!(!ts.flag(flags::H));
    assert!(!ts.flag(flags::C));
}

#[test]
fn test_cp_a_b_equal() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x42);
    ts.set_reg(reg::B, 0x42);
    // CP B (0xB8); JP 0x0200
    ts.write_rom(0x0100, &[0xB8, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x42); // A unchanged by CP
    assert!(ts.flag(flags::Z), "Z set (equal)");
    assert!(ts.flag(flags::N), "N set (subtraction)");
    assert!(!ts.flag(flags::C), "C clear (no borrow)");
}

#[test]
fn test_cp_a_b_less() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x10);
    ts.set_reg(reg::B, 0x20);
    // CP B (0xB8); JP 0x0200
    ts.write_rom(0x0100, &[0xB8, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x10); // A unchanged
    assert!(!ts.flag(flags::Z));
    assert!(ts.flag(flags::N));
    assert!(ts.flag(flags::C), "C set (A < B)");
}

// ============================================================
// INC / DEC register
// ============================================================

#[test]
fn test_inc_b() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::B, 0x0F);
    // INC B (0x04); JP 0x0200
    ts.write_rom(0x0100, &[0x04, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::B), 0x10);
    assert!(!ts.flag(flags::Z));
    assert!(!ts.flag(flags::N));
    assert!(ts.flag(flags::H), "H set (0xF + 1 overflows nibble)");
}

#[test]
fn test_inc_a_overflow() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0xFF);
    // INC A (0x3C); JP 0x0200
    ts.write_rom(0x0100, &[0x3C, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x00);
    assert!(ts.flag(flags::Z), "Z set on wrap to 0");
    assert!(ts.flag(flags::H), "H set (0xF + 1)");
}

#[test]
fn test_dec_b() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::B, 0x10);
    // DEC B (0x05); JP 0x0200
    ts.write_rom(0x0100, &[0x05, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::B), 0x0F);
    assert!(!ts.flag(flags::Z));
    assert!(ts.flag(flags::N), "N set (decrement)");
    assert!(ts.flag(flags::H), "H set (borrow from bit 4)");
}

#[test]
fn test_dec_a_to_zero() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x01);
    // DEC A (0x3D); JP 0x0200
    ts.write_rom(0x0100, &[0x3D, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x00);
    assert!(ts.flag(flags::Z));
    assert!(ts.flag(flags::N));
    assert!(!ts.flag(flags::H), "no half-borrow for 1-1");
}

// ============================================================
// Immediate loads
// ============================================================

#[test]
fn test_ld_a_immediate() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // LD A,0xAB (0x3E, 0xAB); JP 0x0200
    ts.write_rom(0x0100, &[0x3E, 0xAB, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0xAB);
}

#[test]
fn test_ld_bc_immediate() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // LD BC,0x1234 (0x01, 0x34, 0x12); JP 0x0200
    ts.write_rom(0x0100, &[0x01, 0x34, 0x12, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::B), 0x12);
    assert_eq!(ts.get_reg(reg::C), 0x34);
}

// ============================================================
// Memory access (LD r,(HL) / LD (HL),r)
// ============================================================

#[test]
fn test_ld_a_hl_mem() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::H, 0xC0);
    ts.set_reg(reg::L, 0x00);
    ts.memory[0xC000] = 0x77; // Put value in memory
    // LD A,(HL) (0x7E); JP 0x0200
    ts.write_rom(0x0100, &[0x7E, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x77);
}

#[test]
fn test_ld_hl_mem_a() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x55);
    ts.set_reg(reg::H, 0xC0);
    ts.set_reg(reg::L, 0x10);
    // LD (HL),A (0x77); JP 0x0200
    ts.write_rom(0x0100, &[0x77, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.read_mem(0xC010), 0x55);
}

#[test]
fn test_ld_a_bc_mem() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::B, 0xC0);
    ts.set_reg(reg::C, 0x20);
    ts.memory[0xC020] = 0xAB;
    // LD A,(BC) (0x0A); JP 0x0200
    ts.write_rom(0x0100, &[0x0A, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0xAB);
}

// ============================================================
// Stack operations (PUSH / POP)
// ============================================================

#[test]
fn test_push_pop_bc() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::B, 0x12);
    ts.set_reg(reg::C, 0x34);
    // PUSH BC (0xC5); LD B,0 (0x06,0x00); LD C,0 (0x0E,0x00); POP DE (0xD1); JP 0x0200
    ts.write_rom(0x0100, &[
        0xC5,             // PUSH BC
        0x06, 0x00,       // LD B,0
        0x0E, 0x00,       // LD C,0
        0xD1,             // POP DE  (DE gets the value that was BC)
        0xC3, 0x00, 0x02, // JP 0x0200
    ]);
    ts.set_sp(0xDFFF);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::D), 0x12, "D should have old B value");
    assert_eq!(ts.get_reg(reg::E), 0x34, "E should have old C value");
    assert_eq!(ts.get_reg(reg::B), 0x00, "B should be zeroed");
    assert_eq!(ts.get_reg(reg::C), 0x00, "C should be zeroed");
    assert_eq!(ts.get_sp(), 0xDFFF, "SP should be restored after push+pop");
}

// ============================================================
// I/O exits (LDH instructions)
// ============================================================

#[test]
fn test_ldh_io_write_exit() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x42);
    // LDH (0x40),A  → writes A to 0xFF40 (LCDC register)
    // This should exit with IoWrite since 0xFF40 is in I/O range
    ts.write_rom(0x0100, &[0xE0, 0x40]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::IoWrite, "should exit for I/O write");
    assert_eq!(ts.get_io_addr(), 0xFF40, "io_addr should be 0xFF40");
    // PC should point at the LDH instruction (Haskell will re-handle it)
    assert_eq!(ts.get_pc(), 0x0100);
}

#[test]
fn test_ldh_io_read_exit() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // LDH A,(0x44) → reads from 0xFF44 (LY register)
    ts.write_rom(0x0100, &[0xF0, 0x44]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::IoRead, "should exit for I/O read");
    assert_eq!(ts.get_io_addr(), 0xFF44, "io_addr should be 0xFF44");
    assert_eq!(ts.get_pc(), 0x0100);
}

// ============================================================
// CALL / RET
// ============================================================

#[test]
fn test_call() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // CALL 0x0200 (0xCD, 0x00, 0x02)
    ts.write_rom(0x0100, &[0xCD, 0x00, 0x02]);
    ts.set_sp(0xDFFF);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    assert_eq!(ts.get_pc(), 0x0200, "PC should be at call target");
    assert_eq!(ts.get_sp(), 0xDFFD, "SP should be decremented by 2");
    // Return address (0x0103) should be pushed onto stack
    assert_eq!(ts.read_mem(0xDFFD), 0x03, "low byte of return addr");
    assert_eq!(ts.read_mem(0xDFFE), 0x01, "high byte of return addr");
}

#[test]
fn test_ret() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // RET (0xC9) — pops return address from stack
    ts.write_rom(0x0100, &[0xC9]);
    ts.set_sp(0xDFFD);
    // Push a return address onto the stack: 0x0200
    ts.memory[0xDFFD] = 0x00; // low byte
    ts.memory[0xDFFE] = 0x02; // high byte

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    assert_eq!(ts.get_pc(), 0x0200, "PC should be popped return address");
    assert_eq!(ts.get_sp(), 0xDFFF, "SP should be incremented by 2");
}

// ============================================================
// Conditional branches
// ============================================================

#[test]
fn test_jr_z_taken() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // Set Z flag
    ts.set_reg(reg::F, 0x80); // Z=1, N=0, H=0, C=0
    // JR Z,+3 (0x28, 0x03) → jumps to 0x0100 + 2 + 3 = 0x0105
    ts.write_rom(0x0100, &[0x28, 0x03]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    assert_eq!(ts.get_pc(), 0x0105);
    // Branch taken: 8+4=12 T-cycles
    assert_eq!(ts.get_cycles(), 12);
}

#[test]
fn test_jr_z_not_taken() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // Clear Z flag
    ts.set_reg(reg::F, 0x00);
    // JR Z,+3 (0x28, 0x03) — not taken, exits block at next PC
    ts.write_rom(0x0100, &[0x28, 0x03, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    // Conditional branches always end the JIT block; not-taken exits at next instruction
    assert_eq!(ts.get_pc(), 0x0102, "should exit at instruction after JR Z");
    assert_eq!(ts.get_cycles(), 8, "JR Z not-taken = 8 T-cycles");
}

#[test]
fn test_jr_nz_taken() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::F, 0x00); // Z=0
    // JR NZ,+5 (0x20, 0x05) → 0x0100 + 2 + 5 = 0x0107
    ts.write_rom(0x0100, &[0x20, 0x05]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    assert_eq!(ts.get_pc(), 0x0107);
}

// ============================================================
// Downcount exit
// ============================================================

#[test]
fn test_downcount_exit() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // A sequence of NOPs that exceeds the downcount
    // NOP NOP NOP NOP NOP JP 0x0200
    ts.write_rom(0x0100, &[0x00, 0x00, 0x00, 0x00, 0x00, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    // downcount=8: first 2 NOPs use 8 cycles, then downcount triggers
    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 8);
    assert_eq!(exit, ExitReason::Downcount);
    assert_eq!(ts.get_pc(), 0x0102, "PC after 2 NOPs");
    assert_eq!(ts.get_cycles(), 8);
}

// ============================================================
// Multi-instruction sequences
// ============================================================

#[test]
fn test_add_immediate() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // LD A,0x10; ADD A,0x05; JP 0x0200
    ts.write_rom(0x0100, &[
        0x3E, 0x10, // LD A,0x10
        0xC6, 0x05, // ADD A,0x05
        0xC3, 0x00, 0x02,
    ]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x15);
    assert!(!ts.flag(flags::Z));
    assert!(!ts.flag(flags::H));
    assert!(!ts.flag(flags::C));
}

#[test]
fn test_loop_counting() {
    // LD B,3; DEC B; JR NZ,-2  (loop: decrement B until 0)
    // This tests that blocks compile/run correctly when re-entered
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.write_rom(0x0100, &[
        0x06, 0x03, // LD B,3
        0x05,       // DEC B       (at 0x0102)
        0x20, 0xFD, // JR NZ,-3   (at 0x0103, jumps back to 0x0102)
    ]);
    ts.set_sp(0xFFFE);

    // First block: LD B,3; DEC B; JR NZ → taken (B=2, jumps to 0x0102)
    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::BlockEnd);
    assert_eq!(ts.get_reg(reg::B), 0x02);
    assert_eq!(ts.get_pc(), 0x0102);
}

// ============================================================
// Rotate instructions
// ============================================================

#[test]
fn test_rlca() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x85); // 1000_0101
    // RLCA (0x07); JP 0x0200
    ts.write_rom(0x0100, &[0x07, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    // Rotate left: 1000_0101 → 0000_1011, old bit 7 → C
    assert_eq!(ts.get_reg(reg::A), 0x0B);
    assert!(ts.flag(flags::C), "C = old bit 7");
    assert!(!ts.flag(flags::Z), "Z always 0 for RLCA");
    assert!(!ts.flag(flags::N));
    assert!(!ts.flag(flags::H));
}

#[test]
fn test_rrca() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x3B); // 0011_1011
    // RRCA (0x0F); JP 0x0200
    ts.write_rom(0x0100, &[0x0F, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    // Rotate right: 0011_1011 → 1001_1101, old bit 0 → C
    assert_eq!(ts.get_reg(reg::A), 0x9D);
    assert!(ts.flag(flags::C), "C = old bit 0");
}

// ============================================================
// SCF / CCF / CPL
// ============================================================

#[test]
fn test_scf() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::F, 0x00); // all flags clear
    // SCF (0x37); JP 0x0200
    ts.write_rom(0x0100, &[0x37, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert!(ts.flag(flags::C), "C set by SCF");
    assert!(!ts.flag(flags::N), "N cleared by SCF");
    assert!(!ts.flag(flags::H), "H cleared by SCF");
}

#[test]
fn test_ccf() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::F, 0x10); // C=1
    // CCF (0x3F); JP 0x0200
    ts.write_rom(0x0100, &[0x3F, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert!(!ts.flag(flags::C), "C complemented by CCF");
    assert!(!ts.flag(flags::N));
    assert!(!ts.flag(flags::H));
}

#[test]
fn test_cpl() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    ts.set_reg(reg::A, 0x35);
    // CPL (0x2F); JP 0x0200
    ts.write_rom(0x0100, &[0x2F, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0xCA); // ~0x35 = 0xCA
    assert!(ts.flag(flags::N), "N set by CPL");
    assert!(ts.flag(flags::H), "H set by CPL");
}

// ============================================================
// HALT
// ============================================================

#[test]
fn test_halt() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // HALT (0x76)
    ts.write_rom(0x0100, &[0x76]);
    ts.set_sp(0xFFFE);

    let exit = compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(exit, ExitReason::Halt);
}

// ============================================================
// Cycle counting accuracy
// ============================================================

#[test]
fn test_cycle_counting() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();
    // LD A,0x10 (8); LD B,A (4); INC B (4); ADD A,B (4); JP 0x0200 (16)
    ts.write_rom(0x0100, &[
        0x3E, 0x10, // LD A,0x10   = 8 cycles
        0x47,       // LD B,A      = 4 cycles
        0x04,       // INC B       = 4 cycles
        0x80,       // ADD A,B     = 4 cycles
        0xC3, 0x00, 0x02, // JP   = 16 cycles
    ]);
    ts.set_sp(0xFFFE);

    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_cycles(), 8 + 4 + 4 + 4 + 16);
    assert_eq!(ts.get_reg(reg::A), 0x21); // 0x10 + 0x11 = 0x21
    assert_eq!(ts.get_reg(reg::B), 0x11); // 0x10 incremented
}

// ============================================================
// Invalidation
// ============================================================

#[test]
fn test_invalidate_and_recompile() {
    let mut engine = JITEngine::new();
    let mut ts = TestState::new();

    // First version: LD A,0x11; JP 0x0200
    ts.write_rom(0x0100, &[0x3E, 0x11, 0xC3, 0x00, 0x02]);
    ts.set_sp(0xFFFE);
    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x11);

    // Invalidate and change ROM
    engine.invalidate(0x0100);
    ts.write_rom(0x0100, &[0x3E, 0x22, 0xC3, 0x00, 0x02]);

    // Recompile with new code
    compile_and_run(&mut engine, &mut ts, 0x0100, 1000);
    assert_eq!(ts.get_reg(reg::A), 0x22);
}
