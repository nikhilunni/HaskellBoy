use std::collections::HashMap;

use cranelift_codegen::isa::{self, CallConv};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;
use target_lexicon::Triple;

use crate::compiler;
use crate::state::ExitReason;

/// A compiled basic block: a native function pointer.
/// Signature: fn(*mut u8 /* EmulatorState* */, u16 /* downcount */) -> u8 /* ExitReason */
type CompiledBlock = unsafe extern "C" fn(*mut u8, u16) -> u8;

pub struct JITEngine {
    /// Cranelift JIT module for compiling new functions
    pub(crate) module: JITModule,
    /// Cache of compiled blocks: PC → native function pointer
    blocks: HashMap<u16, CompiledBlock>,
    /// Monotonic counter for unique function names (needed for recompilation)
    compile_counter: u32,
}

impl JITEngine {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("opt_level", "speed").unwrap();
        flag_builder.set("is_pic", "false").unwrap();

        let isa_builder = isa::lookup(Triple::host()).expect("unsupported host architecture");
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        JITEngine {
            module,
            blocks: HashMap::new(),
            compile_counter: 0,
        }
    }

    /// Compile a basic block starting at `pc`.
    /// `rom` points to the full 64K memory image, `rom_len` is its length.
    /// Returns 0 on success, nonzero on error.
    pub fn compile_block(&mut self, rom: &[u8], pc: u16) -> Result<(), String> {
        if self.blocks.contains_key(&pc) {
            return Ok(()); // Already compiled
        }

        let version = self.compile_counter;
        self.compile_counter += 1;
        let func_id = compiler::compile_block(&mut self.module, rom, pc, version)?;

        // Finalize all pending functions
        self.module.finalize_definitions().map_err(|e| format!("{e}"))?;

        // Get the function pointer
        let code_ptr = self.module.get_finalized_function(func_id);
        let block_fn: CompiledBlock = unsafe { std::mem::transmute(code_ptr) };

        self.blocks.insert(pc, block_fn);
        Ok(())
    }

    /// Run a compiled block. Returns the exit reason.
    pub fn run_block(&self, state: *mut u8, downcount: u16) -> ExitReason {
        // Read PC from state to find the right block
        let pc = unsafe {
            let pc_ptr = state.offset(crate::state::state_offsets::PC as isize) as *const u16;
            *pc_ptr
        };

        if let Some(&block_fn) = self.blocks.get(&pc) {
            let result = unsafe { block_fn(state, downcount) };
            match result {
                0 => ExitReason::BlockEnd,
                1 => ExitReason::Downcount,
                2 => ExitReason::IoRead,
                3 => ExitReason::IoWrite,
                4 => ExitReason::Halt,
                _ => ExitReason::NotCompiled,
            }
        } else {
            ExitReason::NotCompiled
        }
    }

    /// Check if we have a compiled block for this PC.
    pub fn has_block(&self, pc: u16) -> bool {
        self.blocks.contains_key(&pc)
    }

    /// Invalidate a compiled block at the given PC.
    pub fn invalidate(&mut self, pc: u16) {
        self.blocks.remove(&pc);
    }

    /// Invalidate all compiled blocks.
    pub fn invalidate_all(&mut self) {
        self.blocks.clear();
    }

    /// Get the call convention for our target
    pub fn call_conv(&self) -> CallConv {
        self.module.isa().default_call_conv()
    }
}
