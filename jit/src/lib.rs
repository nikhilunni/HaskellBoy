pub mod state;
pub mod engine;
pub mod compiler;

use engine::JITEngine;
use state::ExitReason;

/// Create a new JIT engine. Returns a heap-allocated pointer.
#[no_mangle]
pub extern "C" fn jit_engine_new() -> *mut JITEngine {
    let engine = Box::new(JITEngine::new());
    Box::into_raw(engine)
}

/// Free a JIT engine.
#[no_mangle]
pub unsafe extern "C" fn jit_engine_free(engine: *mut JITEngine) {
    if !engine.is_null() {
        drop(Box::from_raw(engine));
    }
}

/// Compile a basic block starting at `pc` from ROM bytes.
/// Returns 0 on success, nonzero on error.
#[no_mangle]
pub unsafe extern "C" fn jit_compile_block(
    engine: *mut JITEngine,
    rom: *const u8,
    pc: u16,
    rom_len: u32,
) -> i32 {
    if engine.is_null() || rom.is_null() {
        return -1;
    }
    let engine = &mut *engine;
    let rom_slice = std::slice::from_raw_parts(rom, rom_len as usize);

    match engine.compile_block(rom_slice, pc) {
        Ok(()) => 0,
        Err(_e) => {
            #[cfg(debug_assertions)]
            eprintln!("JIT compile error at PC={:04X}: {}", pc, _e);
            -1
        }
    }
}

/// Run a compiled block. Returns exit reason.
/// The PC is read from the EmulatorState to find the right block.
#[no_mangle]
pub unsafe extern "C" fn jit_run_block(
    engine: *mut JITEngine,
    state: *mut u8,
    downcount: u16,
) -> u8 {
    if engine.is_null() || state.is_null() {
        return ExitReason::NotCompiled as u8;
    }
    let engine = &*engine;
    engine.run_block(state, downcount) as u8
}

/// Check if a compiled block exists for this PC.
#[no_mangle]
pub unsafe extern "C" fn jit_has_block(engine: *mut JITEngine, pc: u16) -> u8 {
    if engine.is_null() {
        return 0;
    }
    let engine = &*engine;
    if engine.has_block(pc) { 1 } else { 0 }
}

/// Invalidate a compiled block at the given PC.
#[no_mangle]
pub unsafe extern "C" fn jit_invalidate(engine: *mut JITEngine, pc: u16) {
    if !engine.is_null() {
        let engine = &mut *engine;
        engine.invalidate(pc);
    }
}

/// Invalidate all compiled blocks.
#[no_mangle]
pub unsafe extern "C" fn jit_invalidate_all(engine: *mut JITEngine) {
    if !engine.is_null() {
        let engine = &mut *engine;
        engine.invalidate_all();
    }
}
