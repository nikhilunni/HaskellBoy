use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::types::*;
use cranelift_codegen::ir::{AbiParam, Function, InstBuilder, MemFlags, Signature, Value};
use cranelift_codegen::Context;
#[allow(unused_imports)]
use cranelift_entity::EntityRef;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_jit::JITModule;

use crate::state::*;
use crate::state::state_offsets as off;

// Cranelift variables for frequently-used values
const VAR_STATE_PTR: u32 = 0;
const VAR_MEM_PTR: u32 = 1;
const VAR_REG_PTR: u32 = 2;
const VAR_CYCLES: u32 = 3;
const VAR_DOWNCOUNT: u32 = 4;
const VAR_PC: u32 = 5;
const VAR_SP: u32 = 6;

/// Helper: build flag byte from individual flag booleans (Z, N, H, C in bits 7,6,5,4)
fn build_flags(builder: &mut FunctionBuilder, z: Value, n: Value, h: Value, c: Value) -> Value {
    let z_shifted = builder.ins().ishl_imm(z, 7);
    let n_shifted = builder.ins().ishl_imm(n, 6);
    let h_shifted = builder.ins().ishl_imm(h, 5);
    let c_shifted = builder.ins().ishl_imm(c, 4);
    let zn = builder.ins().bor(z_shifted, n_shifted);
    let hc = builder.ins().bor(h_shifted, c_shifted);
    builder.ins().bor(zn, hc)
}

/// Helper: load a register value (8-bit) from the registers array
fn load_reg(builder: &mut FunctionBuilder, reg_ptr: Value, idx: u8) -> Value {
    let offset = idx as i32;
    builder.ins().load(I8, MemFlags::trusted(), reg_ptr, offset)
}

/// Helper: store a register value (8-bit) to the registers array
fn store_reg(builder: &mut FunctionBuilder, reg_ptr: Value, idx: u8, val: Value) {
    let offset = idx as i32;
    builder.ins().store(MemFlags::trusted(), val, reg_ptr, offset);
}

/// Helper: load F register and extract flag bits
fn load_flag(builder: &mut FunctionBuilder, reg_ptr: Value, flag_bit: u8) -> Value {
    let f = load_reg(builder, reg_ptr, reg::F);
    let f_wide = builder.ins().uextend(I32, f);
    let shifted = builder.ins().ushr_imm(f_wide, flag_bit as i64);
    builder.ins().band_imm(shifted, 1)
}

/// Helper: compute zero flag from an 8-bit result
fn compute_z(builder: &mut FunctionBuilder, result: Value) -> Value {
    let zero = builder.ins().iconst(I8, 0);
    // icmp returns i8 (0 or 1), which is what we need for build_flags
    builder.ins().icmp(IntCC::Equal, result, zero)
}

/// Helper: add cycles
fn add_cycles(builder: &mut FunctionBuilder, cycles_var: Variable, amount: u16) {
    let old = builder.use_var(cycles_var);
    let inc = builder.ins().iconst(I16, amount as i64);
    let new = builder.ins().iadd(old, inc);
    builder.def_var(cycles_var, new);
}

/// Helper: emit downcount check — if cycles >= downcount, exit
fn check_downcount(
    builder: &mut FunctionBuilder,
    cycles_var: Variable,
    dc_var: Variable,
    state_ptr_var: Variable,
    pc_var: Variable,
    sp_var: Variable,
    continue_block: cranelift_codegen::ir::Block,
) {
    let cycles = builder.use_var(cycles_var);
    let dc = builder.use_var(dc_var);
    let over = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, cycles, dc);

    let exit_block = builder.create_block();
    builder.ins().brif(over, exit_block, &[], continue_block, &[]);

    builder.switch_to_block(exit_block);
    // Write back state before returning on downcount
    let state_ptr = builder.use_var(state_ptr_var);
    write_back_state(builder, state_ptr, cycles_var, pc_var, sp_var);
    let exit_code = builder.ins().iconst(I8, ExitReason::Downcount as i64);
    builder.ins().return_(&[exit_code]);

    builder.switch_to_block(continue_block);
}

/// Helper: write back state before returning.
/// Stores cycles, PC, SP back to the EmulatorState struct.
fn write_back_state(
    builder: &mut FunctionBuilder,
    state_ptr: Value,
    cycles_var: Variable,
    pc_var: Variable,
    sp_var: Variable,
) {
    let cycles = builder.use_var(cycles_var);
    builder.ins().store(MemFlags::trusted(), cycles, state_ptr, off::CYCLES_USED);
    let pc = builder.use_var(pc_var);
    builder.ins().store(MemFlags::trusted(), pc, state_ptr, off::PC);
    let sp = builder.use_var(sp_var);
    builder.ins().store(MemFlags::trusted(), sp, state_ptr, off::SP);
}

/// Helper: emit exit with write-back
fn emit_exit(
    builder: &mut FunctionBuilder,
    reason: ExitReason,
    state_ptr_var: Variable,
    cycles_var: Variable,
    pc_var: Variable,
    sp_var: Variable,
) {
    let state_ptr = builder.use_var(state_ptr_var);
    write_back_state(builder, state_ptr, cycles_var, pc_var, sp_var);
    let exit_code = builder.ins().iconst(I8, reason as i64);
    builder.ins().return_(&[exit_code]);
}

/// Helper: emit I/O check for memory read. Returns (value, needs_fallthrough_block).
/// If the address is in I/O range, exits. Otherwise loads from memory directly.
fn emit_mem_read(
    builder: &mut FunctionBuilder,
    mem_ptr: Value,
    addr: Value, // I32
    state_ptr_var: Variable,
    cycles_var: Variable,
    pc_var: Variable,
    sp_var: Variable,
) -> Value {
    // Exit for: I/O (0xFF00-0xFF7F), IE (0xFFFF)
    // Banked ROM (0x4000-0x7FFF) is synced to memory[] by Haskell on bank switch.
    let io_start = builder.ins().iconst(I32, 0xFF00);
    let io_end = builder.ins().iconst(I32, 0xFF80);
    let ie_addr = builder.ins().iconst(I32, 0xFFFF);

    let ge_start = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, addr, io_start);
    let lt_end = builder.ins().icmp(IntCC::UnsignedLessThan, addr, io_end);
    let in_io = builder.ins().band(ge_start, lt_end);
    let is_ie = builder.ins().icmp(IntCC::Equal, addr, ie_addr);
    let needs_exit = builder.ins().bor(in_io, is_ie);

    let exit_block = builder.create_block();
    let normal_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, I8);

    builder.ins().brif(needs_exit, exit_block, &[], normal_block, &[]);

    // Exit path: store io_addr and return IO_READ
    builder.switch_to_block(exit_block);
    let state_ptr = builder.use_var(state_ptr_var);
    let addr_16 = builder.ins().ireduce(I16, addr);
    builder.ins().store(MemFlags::trusted(), addr_16, state_ptr, off::IO_ADDR);
    write_back_state(builder, state_ptr, cycles_var, pc_var, sp_var);
    let exit_code = builder.ins().iconst(I8, ExitReason::IoRead as i64);
    builder.ins().return_(&[exit_code]);

    // Normal path: direct memory load
    builder.switch_to_block(normal_block);
    // We need to index: mem_ptr + addr
    let addr_native = if cfg!(target_pointer_width = "64") {
        builder.ins().uextend(I64, addr)
    } else {
        addr
    };
    let effective = builder.ins().iadd(mem_ptr, addr_native);
    let val = builder.ins().load(I8, MemFlags::new(), effective, 0);
    builder.ins().jump(merge_block, &[val]);

    builder.switch_to_block(merge_block);
    builder.block_params(merge_block)[0]
}

/// Helper: emit I/O check for memory write.
fn emit_mem_write(
    builder: &mut FunctionBuilder,
    mem_ptr: Value,
    addr: Value, // I32
    val: Value,  // I8
    state_ptr_var: Variable,
    cycles_var: Variable,
    pc_var: Variable,
    sp_var: Variable,
) {
    // Exit for: I/O (0xFF00-0xFF7F), IE (0xFFFF), ROM/MBC area (0x0000-0x7FFF)
    let io_start = builder.ins().iconst(I32, 0xFF00);
    let io_end = builder.ins().iconst(I32, 0xFF80);
    let ie_addr = builder.ins().iconst(I32, 0xFFFF);

    let ge_start = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, addr, io_start);
    let lt_end = builder.ins().icmp(IntCC::UnsignedLessThan, addr, io_end);
    let in_io = builder.ins().band(ge_start, lt_end);
    let is_ie = builder.ins().icmp(IntCC::Equal, addr, ie_addr);

    // ROM/MBC area: writes to 0x0000-0x7FFF handle RAM enable and bank switching
    let rom_end = builder.ins().iconst(I32, 0x8000);
    let in_rom = builder.ins().icmp(IntCC::UnsignedLessThan, addr, rom_end);

    let needs_exit = builder.ins().bor(in_io, is_ie);
    let needs_exit = builder.ins().bor(needs_exit, in_rom);

    let exit_block = builder.create_block();
    let normal_block = builder.create_block();

    builder.ins().brif(needs_exit, exit_block, &[], normal_block, &[]);

    // Exit path: store io_addr, io_value and return IO_WRITE
    builder.switch_to_block(exit_block);
    let state_ptr = builder.use_var(state_ptr_var);
    let addr_16 = builder.ins().ireduce(I16, addr);
    builder.ins().store(MemFlags::trusted(), addr_16, state_ptr, off::IO_ADDR);
    builder.ins().store(MemFlags::trusted(), val, state_ptr, off::IO_VALUE);
    write_back_state(builder, state_ptr, cycles_var, pc_var, sp_var);
    let exit_code = builder.ins().iconst(I8, ExitReason::IoWrite as i64);
    builder.ins().return_(&[exit_code]);

    // Normal path: direct memory store
    builder.switch_to_block(normal_block);
    let addr_native = if cfg!(target_pointer_width = "64") {
        builder.ins().uextend(I64, addr)
    } else {
        addr
    };
    let effective = builder.ins().iadd(mem_ptr, addr_native);
    builder.ins().store(MemFlags::new(), val, effective, 0);
}

/// Helper: load 16-bit register pair (high, low) as I32
fn load_reg_pair(builder: &mut FunctionBuilder, reg_ptr: Value, hi: u8, lo: u8) -> Value {
    let h = load_reg(builder, reg_ptr, hi);
    let l = load_reg(builder, reg_ptr, lo);
    let h32 = builder.ins().uextend(I32, h);
    let l32 = builder.ins().uextend(I32, l);
    let h_shifted = builder.ins().ishl_imm(h32, 8);
    builder.ins().bor(h_shifted, l32)
}

/// Helper: store 16-bit value as register pair
fn store_reg_pair(builder: &mut FunctionBuilder, reg_ptr: Value, hi: u8, lo: u8, val: Value) {
    // val is I32
    let hi_val = builder.ins().ushr_imm(val, 8);
    let hi_byte = builder.ins().ireduce(I8, hi_val);
    let lo_byte = builder.ins().ireduce(I8, val);
    store_reg(builder, reg_ptr, hi, hi_byte);
    store_reg(builder, reg_ptr, lo, lo_byte);
}

/// Map opcode register index to our register constants.
/// For opcodes 0x40-0xBF, bits [5:3] = dest, bits [2:0] = src
/// Index 6 = (HL) which is memory, not a register.
fn opcode_reg(idx: u8) -> Option<u8> {
    match idx {
        0 => Some(reg::B),
        1 => Some(reg::C),
        2 => Some(reg::D),
        3 => Some(reg::E),
        4 => Some(reg::H),
        5 => Some(reg::L),
        6 => None, // (HL)
        7 => Some(reg::A),
        _ => None,
    }
}

/// Compile a basic block starting at `start_pc` from memory bytes.
/// Returns a FuncId for the compiled function.
pub fn compile_block(
    module: &mut JITModule,
    mem: &[u8],
    start_pc: u16,
    version: u32,
) -> Result<FuncId, String> {
    let call_conv = module.isa().default_call_conv();

    // Function signature: fn(state: *mut u8, downcount: u16) -> u8
    let mut sig = Signature::new(call_conv);
    let ptr_type = module.isa().pointer_type();
    sig.params.push(AbiParam::new(ptr_type)); // state pointer
    sig.params.push(AbiParam::new(I16));       // downcount
    sig.returns.push(AbiParam::new(I8));       // exit reason

    let name = format!("block_{:04X}_v{}", start_pc, version);
    let func_id = module
        .declare_function(&name, Linkage::Local, &sig)
        .map_err(|e| format!("{e}"))?;

    let mut func = Function::with_name_signature(
        cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32()),
        sig,
    );

    let mut func_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut func, &mut func_ctx);

    // Create entry block
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    // Declare variables
    let state_ptr_var = Variable::from_u32(VAR_STATE_PTR);
    let mem_ptr_var = Variable::from_u32(VAR_MEM_PTR);
    let reg_ptr_var = Variable::from_u32(VAR_REG_PTR);
    let cycles_var = Variable::from_u32(VAR_CYCLES);
    let dc_var = Variable::from_u32(VAR_DOWNCOUNT);
    let pc_var = Variable::from_u32(VAR_PC);
    let sp_var = Variable::from_u32(VAR_SP);

    builder.declare_var(state_ptr_var, ptr_type);
    builder.declare_var(mem_ptr_var, ptr_type);
    builder.declare_var(reg_ptr_var, ptr_type);
    builder.declare_var(cycles_var, I16);
    builder.declare_var(dc_var, I16);
    builder.declare_var(pc_var, I16);
    builder.declare_var(sp_var, I16);

    // Initialize variables from function params
    let state_ptr_param = builder.block_params(entry_block)[0];
    let dc_param = builder.block_params(entry_block)[1];

    builder.def_var(state_ptr_var, state_ptr_param);
    builder.def_var(dc_var, dc_param);

    // Load memory and register pointers from state
    let mem_ptr_val = builder.ins().load(ptr_type, MemFlags::trusted(), state_ptr_param, off::MEMORY);
    builder.def_var(mem_ptr_var, mem_ptr_val);

    let reg_ptr_val = builder.ins().load(ptr_type, MemFlags::trusted(), state_ptr_param, off::REGISTERS);
    builder.def_var(reg_ptr_var, reg_ptr_val);

    // Load initial cycles, PC, SP
    let init_cycles = builder.ins().load(I16, MemFlags::trusted(), state_ptr_param, off::CYCLES_USED);
    builder.def_var(cycles_var, init_cycles);

    let init_pc = builder.ins().load(I16, MemFlags::trusted(), state_ptr_param, off::PC);
    builder.def_var(pc_var, init_pc);

    let init_sp = builder.ins().load(I16, MemFlags::trusted(), state_ptr_param, off::SP);
    builder.def_var(sp_var, init_sp);

    // Walk opcodes and emit IR
    let mut pc = start_pc;
    let mut first_instr = true;
    let mut block_terminated = false;

    loop {
        if pc as usize >= mem.len() {
            break;
        }

        let op = mem[pc as usize];

        // After the first instruction, check downcount before each new instruction
        if !first_instr {
            let cont_block = builder.create_block();
            check_downcount(&mut builder, cycles_var, dc_var, state_ptr_var, pc_var, sp_var, cont_block);
            builder.seal_block(cont_block);
        }
        first_instr = false;

        // Update PC variable to point to current instruction
        let pc_val = builder.ins().iconst(I16, pc as i64);
        builder.def_var(pc_var, pc_val);

        let len = INSTR_LENGTH[op as usize] as u16;
        let cycles = CYCLE_COST[op as usize];

        // Emit instruction
        let terminated = emit_instruction(
            &mut builder, mem, pc, op,
            state_ptr_var, mem_ptr_var, reg_ptr_var,
            cycles_var, dc_var, pc_var, sp_var,
        );

        if terminated {
            block_terminated = true;
            break;
        }

        // Advance PC past this instruction
        let next_pc_val = builder.ins().iconst(I16, (pc + len) as i64);
        builder.def_var(pc_var, next_pc_val);

        // Add cycle cost
        add_cycles(&mut builder, cycles_var, cycles);

        pc += len;

        // If this is a block terminator, stop
        if is_block_terminator(op) {
            // Emit exit
            emit_exit(
                &mut builder, ExitReason::BlockEnd,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            block_terminated = true;
            break;
        }

        // Safety: don't compile more than 64 instructions per block
        if (pc - start_pc) > 128 {
            emit_exit(
                &mut builder, ExitReason::BlockEnd,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            block_terminated = true;
            break;
        }
    }

    // If we fell through without a terminator, emit block end exit
    if !block_terminated {
        emit_exit(
            &mut builder, ExitReason::BlockEnd,
            state_ptr_var, cycles_var, pc_var, sp_var,
        );
    }

    // Seal all remaining blocks. Cranelift requires every block to be sealed
    // before finalization — unsealed blocks produce undefined/corrupt native code.
    builder.seal_all_blocks();
    builder.finalize();

    // Compile the function
    let mut ctx = Context::for_function(func);
    module
        .define_function(func_id, &mut ctx)
        .map_err(|e| format!("{e:?}"))?;

    Ok(func_id)
}

/// Emit Cranelift IR for a single SM83 instruction.
/// Returns true if the instruction terminates the block (i.e., emits its own return).
fn emit_instruction(
    builder: &mut FunctionBuilder,
    mem: &[u8],
    pc: u16,
    op: u8,
    state_ptr_var: Variable,
    mem_ptr_var: Variable,
    reg_ptr_var: Variable,
    cycles_var: Variable,
    _dc_var: Variable,
    pc_var: Variable,
    sp_var: Variable,
) -> bool {
    let reg_ptr = builder.use_var(reg_ptr_var);
    let mem_ptr = builder.use_var(mem_ptr_var);

    match op {
        // ===== NOP =====
        0x00 => false,

        // ===== LD r,r (0x40-0x7F, excluding 0x76=HALT and (HL) variants) =====
        0x40..=0x7F if op != 0x76 => {
            let dst_idx = (op >> 3) & 7;
            let src_idx = op & 7;

            match (opcode_reg(dst_idx), opcode_reg(src_idx)) {
                (Some(dst), Some(src)) => {
                    // LD r,r
                    let val = load_reg(builder, reg_ptr, src);
                    store_reg(builder, reg_ptr, dst, val);
                    false
                }
                (Some(dst), None) => {
                    // LD r,(HL)
                    let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
                    let val = emit_mem_read(
                        builder, mem_ptr, hl,
                        state_ptr_var, cycles_var, pc_var, sp_var,
                    );
                    store_reg(builder, reg_ptr, dst, val);
                    false
                }
                (None, Some(src)) => {
                    // LD (HL),r
                    let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
                    let val = load_reg(builder, reg_ptr, src);
                    emit_mem_write(
                        builder, mem_ptr, hl, val,
                        state_ptr_var, cycles_var, pc_var, sp_var,
                    );
                    false
                }
                (None, None) => {
                    // HALT (0x76) — handled below
                    false
                }
            }
        }

        // ===== HALT =====
        0x76 => {
            let state_ptr = builder.use_var(state_ptr_var);
            let one = builder.ins().iconst(I8, 1);
            builder.ins().store(MemFlags::trusted(), one, state_ptr, off::HALT);

            // Advance PC past HALT
            let next_pc = builder.ins().iconst(I16, (pc + 1) as i64);
            builder.def_var(pc_var, next_pc);
            add_cycles(builder, cycles_var, CYCLE_COST[0x76]);

            emit_exit(
                builder, ExitReason::Halt,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            true
        }

        // ===== 8-bit ALU with register: ADD/ADC/SUB/SBC/AND/XOR/OR/CP =====
        0x80..=0xBF => {
            let alu_op = (op >> 3) & 7;
            let src_idx = op & 7;

            let src_val = if let Some(src) = opcode_reg(src_idx) {
                load_reg(builder, reg_ptr, src)
            } else {
                // (HL) operand
                let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
                emit_mem_read(
                    builder, mem_ptr, hl,
                    state_ptr_var, cycles_var, pc_var, sp_var,
                )
            };

            emit_alu_op(builder, reg_ptr, alu_op, src_val);
            false
        }

        // ===== ALU A,n (immediate) =====
        0xC6 | 0xCE | 0xD6 | 0xDE | 0xE6 | 0xEE | 0xF6 | 0xFE => {
            let imm = mem[(pc + 1) as usize];
            let imm_val = builder.ins().iconst(I8, imm as i64);
            let alu_op = (op >> 3) & 7;
            emit_alu_op(builder, reg_ptr, alu_op, imm_val);
            false
        }

        // ===== INC r =====
        0x04 | 0x0C | 0x14 | 0x1C | 0x24 | 0x2C | 0x3C => {
            let r_idx = (op >> 3) & 7;
            if let Some(r) = opcode_reg(r_idx) {
                let val = load_reg(builder, reg_ptr, r);
                let val32 = builder.ins().uextend(I32, val);
                let one = builder.ins().iconst(I32, 1);
                let result32 = builder.ins().iadd(val32, one);
                let result = builder.ins().ireduce(I8, result32);
                store_reg(builder, reg_ptr, r, result);

                // Flags: Z=result==0, N=0, H=(val&0xF)+1>0xF, C unchanged
                let z = compute_z(builder, result);
                let n = builder.ins().iconst(I8, 0);
                let low_nib = builder.ins().band_imm(val32, 0xF);
                let low_sum = builder.ins().iadd(low_nib, one);
                let hf_threshold = builder.ins().iconst(I32, 0xF);
                let h_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, low_sum, hf_threshold);
                let h = h_cmp;
                // Keep old carry
                let old_f = load_reg(builder, reg_ptr, reg::F);
                let old_c_bit = builder.ins().ushr_imm(old_f, 4);
                let old_c = builder.ins().band_imm(old_c_bit, 1);
                let flags = build_flags(builder, z, n, h, old_c);
                store_reg(builder, reg_ptr, reg::F, flags);
            }
            false
        }

        // ===== DEC r =====
        0x05 | 0x0D | 0x15 | 0x1D | 0x25 | 0x2D | 0x3D => {
            let r_idx = (op >> 3) & 7;
            if let Some(r) = opcode_reg(r_idx) {
                let val = load_reg(builder, reg_ptr, r);
                let val32 = builder.ins().uextend(I32, val);
                let one = builder.ins().iconst(I32, 1);
                let result32 = builder.ins().isub(val32, one);
                let result = builder.ins().ireduce(I8, result32);
                store_reg(builder, reg_ptr, r, result);

                // Flags: Z=result==0, N=1, H=(val&0xF)==0, C unchanged
                let z = compute_z(builder, result);
                let n = builder.ins().iconst(I8, 1);
                let low_nib = builder.ins().band_imm(val32, 0xF);
                let zero32 = builder.ins().iconst(I32, 0);
                let h_cmp = builder.ins().icmp(IntCC::Equal, low_nib, zero32);
                let h = h_cmp;
                let old_f = load_reg(builder, reg_ptr, reg::F);
                let old_c_bit = builder.ins().ushr_imm(old_f, 4);
                let old_c = builder.ins().band_imm(old_c_bit, 1);
                let flags = build_flags(builder, z, n, h, old_c);
                store_reg(builder, reg_ptr, reg::F, flags);
            }
            false
        }

        // ===== INC (HL) =====
        0x34 => {
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let val = emit_mem_read(
                builder, mem_ptr, hl,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            let val32 = builder.ins().uextend(I32, val);
            let one = builder.ins().iconst(I32, 1);
            let result32 = builder.ins().iadd(val32, one);
            let result = builder.ins().ireduce(I8, result32);
            emit_mem_write(
                builder, mem_ptr, hl, result,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );

            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let low_nib = builder.ins().band_imm(val32, 0xF);
            let low_sum = builder.ins().iadd(low_nib, one);
            let hf_threshold = builder.ins().iconst(I32, 0xF);
            let h_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, low_sum, hf_threshold);
            let h = h_cmp;
            let old_f = load_reg(builder, reg_ptr, reg::F);
            let old_c_bit = builder.ins().ushr_imm(old_f, 4);
            let old_c = builder.ins().band_imm(old_c_bit, 1);
            let flags = build_flags(builder, z, n, h, old_c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== DEC (HL) =====
        0x35 => {
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let val = emit_mem_read(
                builder, mem_ptr, hl,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            let val32 = builder.ins().uextend(I32, val);
            let one = builder.ins().iconst(I32, 1);
            let result32 = builder.ins().isub(val32, one);
            let result = builder.ins().ireduce(I8, result32);
            emit_mem_write(
                builder, mem_ptr, hl, result,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );

            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 1);
            let low_nib = builder.ins().band_imm(val32, 0xF);
            let zero32 = builder.ins().iconst(I32, 0);
            let h_cmp = builder.ins().icmp(IntCC::Equal, low_nib, zero32);
            let h = h_cmp;
            let old_f = load_reg(builder, reg_ptr, reg::F);
            let old_c_bit = builder.ins().ushr_imm(old_f, 4);
            let old_c = builder.ins().band_imm(old_c_bit, 1);
            let flags = build_flags(builder, z, n, h, old_c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== RLCA =====
        0x07 => {
            let a = load_reg(builder, reg_ptr, reg::A);
            let a32 = builder.ins().uextend(I32, a);
            let bit7 = builder.ins().ushr_imm(a32, 7);
            let shifted = builder.ins().ishl_imm(a32, 1);
            let result32 = builder.ins().bor(shifted, bit7);
            let result = builder.ins().ireduce(I8, result32);
            store_reg(builder, reg_ptr, reg::A, result);

            let z = builder.ins().iconst(I8, 0);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit7);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== RRCA =====
        0x0F => {
            let a = load_reg(builder, reg_ptr, reg::A);
            let a32 = builder.ins().uextend(I32, a);
            let bit0 = builder.ins().band_imm(a32, 1);
            let shifted = builder.ins().ushr_imm(a32, 1);
            let bit0_to_7 = builder.ins().ishl_imm(bit0, 7);
            let result32 = builder.ins().bor(shifted, bit0_to_7);
            let result = builder.ins().ireduce(I8, result32);
            store_reg(builder, reg_ptr, reg::A, result);

            let z = builder.ins().iconst(I8, 0);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== RLA =====
        0x17 => {
            let a = load_reg(builder, reg_ptr, reg::A);
            let a32 = builder.ins().uextend(I32, a);
            let old_carry = load_flag(builder, reg_ptr, flags::C);
            let bit7 = builder.ins().ushr_imm(a32, 7);
            let shifted = builder.ins().ishl_imm(a32, 1);
            let result32 = builder.ins().bor(shifted, old_carry);
            let result = builder.ins().ireduce(I8, result32);
            store_reg(builder, reg_ptr, reg::A, result);

            let z = builder.ins().iconst(I8, 0);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit7);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== RRA =====
        0x1F => {
            let a = load_reg(builder, reg_ptr, reg::A);
            let a32 = builder.ins().uextend(I32, a);
            let old_carry = load_flag(builder, reg_ptr, flags::C);
            let bit0 = builder.ins().band_imm(a32, 1);
            let shifted = builder.ins().ushr_imm(a32, 1);
            let carry_to_7 = builder.ins().ishl_imm(old_carry, 7);
            let result32 = builder.ins().bor(shifted, carry_to_7);
            let result = builder.ins().ireduce(I8, result32);
            store_reg(builder, reg_ptr, reg::A, result);

            let z = builder.ins().iconst(I8, 0);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== DAA =====
        0x27 => {
            emit_daa(builder, reg_ptr);
            false
        }

        // ===== CPL =====
        0x2F => {
            let a = load_reg(builder, reg_ptr, reg::A);
            let result = builder.ins().bnot(a);
            store_reg(builder, reg_ptr, reg::A, result);
            // N=1, H=1, Z and C unchanged
            let old_f = load_reg(builder, reg_ptr, reg::F);
            let old_f32 = builder.ins().uextend(I32, old_f);
            let z_bit = builder.ins().ushr_imm(old_f32, 7);
            let z = builder.ins().ireduce(I8, z_bit);
            let z = builder.ins().band_imm(z, 1);
            let c_bit = builder.ins().ushr_imm(old_f32, 4);
            let c = builder.ins().ireduce(I8, c_bit);
            let c = builder.ins().band_imm(c, 1);
            let n = builder.ins().iconst(I8, 1);
            let h = builder.ins().iconst(I8, 1);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== SCF =====
        0x37 => {
            let old_f = load_reg(builder, reg_ptr, reg::F);
            let old_f32 = builder.ins().uextend(I32, old_f);
            let z_bit = builder.ins().ushr_imm(old_f32, 7);
            let z = builder.ins().ireduce(I8, z_bit);
            let z = builder.ins().band_imm(z, 1);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().iconst(I8, 1);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== CCF =====
        0x3F => {
            let old_f = load_reg(builder, reg_ptr, reg::F);
            let old_f32 = builder.ins().uextend(I32, old_f);
            let z_bit = builder.ins().ushr_imm(old_f32, 7);
            let z = builder.ins().ireduce(I8, z_bit);
            let z = builder.ins().band_imm(z, 1);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let old_c = builder.ins().ushr_imm(old_f32, 4);
            let old_c = builder.ins().ireduce(I8, old_c);
            let old_c = builder.ins().band_imm(old_c, 1);
            let one = builder.ins().iconst(I8, 1);
            let c = builder.ins().bxor(old_c, one);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== LD r,n (immediate byte) =====
        0x06 | 0x0E | 0x16 | 0x1E | 0x26 | 0x2E | 0x3E => {
            let r_idx = (op >> 3) & 7;
            let imm = mem[(pc + 1) as usize];
            if let Some(r) = opcode_reg(r_idx) {
                let val = builder.ins().iconst(I8, imm as i64);
                store_reg(builder, reg_ptr, r, val);
            }
            false
        }

        // ===== LD (HL),n =====
        0x36 => {
            let imm = mem[(pc + 1) as usize];
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let val = builder.ins().iconst(I8, imm as i64);
            emit_mem_write(
                builder, mem_ptr, hl, val,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            false
        }

        // ===== LD A,(BC) =====
        0x0A => {
            let bc = load_reg_pair(builder, reg_ptr, reg::B, reg::C);
            let val = emit_mem_read(
                builder, mem_ptr, bc,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            store_reg(builder, reg_ptr, reg::A, val);
            false
        }

        // ===== LD A,(DE) =====
        0x1A => {
            let de = load_reg_pair(builder, reg_ptr, reg::D, reg::E);
            let val = emit_mem_read(
                builder, mem_ptr, de,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            store_reg(builder, reg_ptr, reg::A, val);
            false
        }

        // ===== LD (BC),A =====
        0x02 => {
            let bc = load_reg_pair(builder, reg_ptr, reg::B, reg::C);
            let a = load_reg(builder, reg_ptr, reg::A);
            emit_mem_write(
                builder, mem_ptr, bc, a,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            false
        }

        // ===== LD (DE),A =====
        0x12 => {
            let de = load_reg_pair(builder, reg_ptr, reg::D, reg::E);
            let a = load_reg(builder, reg_ptr, reg::A);
            emit_mem_write(
                builder, mem_ptr, de, a,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            false
        }

        // ===== LD A,(HL+) =====
        0x2A => {
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let val = emit_mem_read(
                builder, mem_ptr, hl,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            store_reg(builder, reg_ptr, reg::A, val);
            // HL++
            let one = builder.ins().iconst(I32, 1);
            let hl_inc = builder.ins().iadd(hl, one);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, hl_inc);
            false
        }

        // ===== LD A,(HL-) =====
        0x3A => {
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let val = emit_mem_read(
                builder, mem_ptr, hl,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            store_reg(builder, reg_ptr, reg::A, val);
            // HL--
            let one = builder.ins().iconst(I32, 1);
            let hl_dec = builder.ins().isub(hl, one);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, hl_dec);
            false
        }

        // ===== LD (HL+),A =====
        0x22 => {
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let a = load_reg(builder, reg_ptr, reg::A);
            emit_mem_write(
                builder, mem_ptr, hl, a,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            let one = builder.ins().iconst(I32, 1);
            let hl_inc = builder.ins().iadd(hl, one);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, hl_inc);
            false
        }

        // ===== LD (HL-),A =====
        0x32 => {
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let a = load_reg(builder, reg_ptr, reg::A);
            emit_mem_write(
                builder, mem_ptr, hl, a,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            let one = builder.ins().iconst(I32, 1);
            let hl_dec = builder.ins().isub(hl, one);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, hl_dec);
            false
        }

        // ===== LD rr,nn (16-bit immediate loads) =====
        0x01 => { // LD BC,nn
            let nn = read_u16(mem, pc + 1);
            let nn_val = builder.ins().iconst(I32, nn as i64);
            store_reg_pair(builder, reg_ptr, reg::B, reg::C, nn_val);
            false
        }
        0x11 => { // LD DE,nn
            let nn = read_u16(mem, pc + 1);
            let nn_val = builder.ins().iconst(I32, nn as i64);
            store_reg_pair(builder, reg_ptr, reg::D, reg::E, nn_val);
            false
        }
        0x21 => { // LD HL,nn
            let nn = read_u16(mem, pc + 1);
            let nn_val = builder.ins().iconst(I32, nn as i64);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, nn_val);
            false
        }
        0x31 => { // LD SP,nn
            let nn = read_u16(mem, pc + 1);
            let sp_val = builder.ins().iconst(I16, nn as i64);
            builder.def_var(sp_var, sp_val);
            false
        }

        // ===== INC rr =====
        0x03 => { // INC BC
            let bc = load_reg_pair(builder, reg_ptr, reg::B, reg::C);
            let one = builder.ins().iconst(I32, 1);
            let bc_inc = builder.ins().iadd(bc, one);
            store_reg_pair(builder, reg_ptr, reg::B, reg::C, bc_inc);
            false
        }
        0x13 => { // INC DE
            let de = load_reg_pair(builder, reg_ptr, reg::D, reg::E);
            let one = builder.ins().iconst(I32, 1);
            let de_inc = builder.ins().iadd(de, one);
            store_reg_pair(builder, reg_ptr, reg::D, reg::E, de_inc);
            false
        }
        0x23 => { // INC HL
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let one = builder.ins().iconst(I32, 1);
            let hl_inc = builder.ins().iadd(hl, one);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, hl_inc);
            false
        }
        0x33 => { // INC SP
            let sp = builder.use_var(sp_var);
            let one = builder.ins().iconst(I16, 1);
            let sp_inc = builder.ins().iadd(sp, one);
            builder.def_var(sp_var, sp_inc);
            false
        }

        // ===== DEC rr =====
        0x0B => { // DEC BC
            let bc = load_reg_pair(builder, reg_ptr, reg::B, reg::C);
            let one = builder.ins().iconst(I32, 1);
            let bc_dec = builder.ins().isub(bc, one);
            store_reg_pair(builder, reg_ptr, reg::B, reg::C, bc_dec);
            false
        }
        0x1B => { // DEC DE
            let de = load_reg_pair(builder, reg_ptr, reg::D, reg::E);
            let one = builder.ins().iconst(I32, 1);
            let de_dec = builder.ins().isub(de, one);
            store_reg_pair(builder, reg_ptr, reg::D, reg::E, de_dec);
            false
        }
        0x2B => { // DEC HL
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let one = builder.ins().iconst(I32, 1);
            let hl_dec = builder.ins().isub(hl, one);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, hl_dec);
            false
        }
        0x3B => { // DEC SP
            let sp = builder.use_var(sp_var);
            let one = builder.ins().iconst(I16, 1);
            let sp_dec = builder.ins().isub(sp, one);
            builder.def_var(sp_var, sp_dec);
            false
        }

        // ===== ADD HL,rr =====
        0x09 => { emit_add_hl_rr(builder, reg_ptr, reg::B, reg::C); false }
        0x19 => { emit_add_hl_rr(builder, reg_ptr, reg::D, reg::E); false }
        0x29 => { // ADD HL,HL
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let result = builder.ins().iadd(hl, hl);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, result);
            // Flags: N=0, H=bit11 carry, C=bit15 carry, Z unchanged
            let old_f = load_reg(builder, reg_ptr, reg::F);
            let old_f32 = builder.ins().uextend(I32, old_f);
            let z_bit = builder.ins().ushr_imm(old_f32, 7);
            let z = builder.ins().ireduce(I8, z_bit);
            let z = builder.ins().band_imm(z, 1);
            let n = builder.ins().iconst(I8, 0);
            // H: (hl & 0xFFF) + (hl & 0xFFF) > 0xFFF
            let low12 = builder.ins().band_imm(hl, 0xFFF);
            let low12_sum = builder.ins().iadd(low12, low12);
            let hf_thr = builder.ins().iconst(I32, 0xFFF);
            let h_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, low12_sum, hf_thr);
            let h = h_cmp;
            // C: result > 0xFFFF
            let cf_thr = builder.ins().iconst(I32, 0xFFFF);
            let c_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, result, cf_thr);
            let c = c_cmp;
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }
        0x39 => { // ADD HL,SP
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let sp = builder.use_var(sp_var);
            let sp32 = builder.ins().uextend(I32, sp);
            let result = builder.ins().iadd(hl, sp32);
            store_reg_pair(builder, reg_ptr, reg::H, reg::L, result);
            let old_f = load_reg(builder, reg_ptr, reg::F);
            let old_f32 = builder.ins().uextend(I32, old_f);
            let z_bit = builder.ins().ushr_imm(old_f32, 7);
            let z = builder.ins().ireduce(I8, z_bit);
            let z = builder.ins().band_imm(z, 1);
            let n = builder.ins().iconst(I8, 0);
            let hl12 = builder.ins().band_imm(hl, 0xFFF);
            let sp12 = builder.ins().band_imm(sp32, 0xFFF);
            let low12_sum = builder.ins().iadd(hl12, sp12);
            let hf_thr = builder.ins().iconst(I32, 0xFFF);
            let h_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, low12_sum, hf_thr);
            let h = h_cmp;
            let cf_thr = builder.ins().iconst(I32, 0xFFFF);
            let c_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, result, cf_thr);
            let c = c_cmp;
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
            false
        }

        // ===== LDH (n),A — always I/O exit =====
        // PC stays at the instruction start; Haskell interpreter replays it.
        0xE0 => {
            // LDH (n),A — write A to 0xFF00+n
            let n = mem[pc as usize + 1] as u16;
            let io_addr = 0xFF00 + n;
            let state_ptr = builder.use_var(state_ptr_var);
            let addr_val = builder.ins().iconst(I16, io_addr as i64);
            builder.ins().store(MemFlags::trusted(), addr_val, state_ptr, off::IO_ADDR);
            let a = load_reg(builder, reg_ptr, reg::A);
            builder.ins().store(MemFlags::trusted(), a, state_ptr, off::IO_VALUE);
            emit_exit(
                builder, ExitReason::IoWrite,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            true
        }

        // ===== LDH A,(n) — always I/O exit =====
        0xF0 => {
            let n = mem[pc as usize + 1] as u16;
            let io_addr = 0xFF00 + n;
            let state_ptr = builder.use_var(state_ptr_var);
            let addr_val = builder.ins().iconst(I16, io_addr as i64);
            builder.ins().store(MemFlags::trusted(), addr_val, state_ptr, off::IO_ADDR);
            emit_exit(
                builder, ExitReason::IoRead,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            true
        }

        // ===== LD (C),A — always I/O exit (0xFF00+C) =====
        0xE2 => {
            let c = load_reg(builder, reg_ptr, reg::C);
            let c32 = builder.ins().uextend(I32, c);
            let base = builder.ins().iconst(I32, 0xFF00);
            let addr32 = builder.ins().iadd(c32, base);
            let addr16 = builder.ins().ireduce(I16, addr32);
            let state_ptr = builder.use_var(state_ptr_var);
            builder.ins().store(MemFlags::trusted(), addr16, state_ptr, off::IO_ADDR);
            let a = load_reg(builder, reg_ptr, reg::A);
            builder.ins().store(MemFlags::trusted(), a, state_ptr, off::IO_VALUE);
            emit_exit(
                builder, ExitReason::IoWrite,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            true
        }

        // ===== LD A,(C) — always I/O exit (0xFF00+C) =====
        0xF2 => {
            let c = load_reg(builder, reg_ptr, reg::C);
            let c32 = builder.ins().uextend(I32, c);
            let base = builder.ins().iconst(I32, 0xFF00);
            let addr32 = builder.ins().iadd(c32, base);
            let addr16 = builder.ins().ireduce(I16, addr32);
            let state_ptr = builder.use_var(state_ptr_var);
            builder.ins().store(MemFlags::trusted(), addr16, state_ptr, off::IO_ADDR);
            emit_exit(
                builder, ExitReason::IoRead,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            true
        }

        // ===== LD A,(nn) =====
        0xFA => {
            let nn = read_u16(mem, pc + 1);
            let addr = builder.ins().iconst(I32, nn as i64);
            let val = emit_mem_read(
                builder, mem_ptr, addr,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            store_reg(builder, reg_ptr, reg::A, val);
            false
        }

        // ===== LD (nn),A =====
        0xEA => {
            let nn = read_u16(mem, pc + 1);
            let addr = builder.ins().iconst(I32, nn as i64);
            let a = load_reg(builder, reg_ptr, reg::A);
            emit_mem_write(
                builder, mem_ptr, addr, a,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            false
        }

        // ===== LD (nn),SP =====
        0x08 => {
            let nn = read_u16(mem, pc + 1);
            let sp = builder.use_var(sp_var);
            let sp32 = builder.ins().uextend(I32, sp);
            // Low byte
            let lo = builder.ins().ireduce(I8, sp32);
            let addr_lo = builder.ins().iconst(I32, nn as i64);
            emit_mem_write(
                builder, mem_ptr, addr_lo, lo,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            // High byte
            let hi32 = builder.ins().ushr_imm(sp32, 8);
            let hi = builder.ins().ireduce(I8, hi32);
            let addr_hi = builder.ins().iconst(I32, (nn + 1) as i64);
            emit_mem_write(
                builder, mem_ptr, addr_hi, hi,
                state_ptr_var, cycles_var, pc_var, sp_var,
            );
            false
        }

        // ===== PUSH rr =====
        0xC5 => { emit_push(builder, reg_ptr, reg::B, reg::C, mem_ptr, sp_var, state_ptr_var, cycles_var, pc_var); false }
        0xD5 => { emit_push(builder, reg_ptr, reg::D, reg::E, mem_ptr, sp_var, state_ptr_var, cycles_var, pc_var); false }
        0xE5 => { emit_push(builder, reg_ptr, reg::H, reg::L, mem_ptr, sp_var, state_ptr_var, cycles_var, pc_var); false }
        0xF5 => { emit_push(builder, reg_ptr, reg::A, reg::F, mem_ptr, sp_var, state_ptr_var, cycles_var, pc_var); false }

        // ===== POP rr =====
        0xC1 => { emit_pop(builder, reg_ptr, reg::B, reg::C, mem_ptr, sp_var, state_ptr_var, cycles_var, pc_var); false }
        0xD1 => { emit_pop(builder, reg_ptr, reg::D, reg::E, mem_ptr, sp_var, state_ptr_var, cycles_var, pc_var); false }
        0xE1 => { emit_pop(builder, reg_ptr, reg::H, reg::L, mem_ptr, sp_var, state_ptr_var, cycles_var, pc_var); false }
        0xF1 => {
            // POP AF — F register lower nibble masked to 0
            emit_pop(builder, reg_ptr, reg::A, reg::F, mem_ptr, sp_var, state_ptr_var, cycles_var, pc_var);
            let f = load_reg(builder, reg_ptr, reg::F);
            let f32 = builder.ins().uextend(I32, f);
            let masked = builder.ins().band_imm(f32, 0xF0);
            let masked8 = builder.ins().ireduce(I8, masked);
            store_reg(builder, reg_ptr, reg::F, masked8);
            false
        }

        // ===== LD SP,HL =====
        0xF9 => {
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let hl16 = builder.ins().ireduce(I16, hl);
            builder.def_var(sp_var, hl16);
            false
        }

        // ===== DI =====
        0xF3 => {
            let state_ptr = builder.use_var(state_ptr_var);
            let zero = builder.ins().iconst(I8, 0);
            builder.ins().store(MemFlags::trusted(), zero, state_ptr, off::IME);
            false
        }

        // ===== EI =====
        0xFB => {
            // Set IME=1 directly in the state struct.
            // The one-instruction delay is effectively preserved because interrupts
            // are only checked by handleInterrupts after the block exits (always at
            // least one instruction later). No need to exit the block.
            let state_ptr = builder.use_var(state_ptr_var);
            let one = builder.ins().iconst(I8, 1);
            builder.ins().store(MemFlags::trusted(), one, state_ptr, off::IME);
            false
        }

        // ===== Control flow — block terminators =====

        // JP nn
        0xC3 => {
            let nn = read_u16(mem, pc + 1);
            let target = builder.ins().iconst(I16, nn as i64);
            builder.def_var(pc_var, target);
            add_cycles(builder, cycles_var, CYCLE_COST[0xC3]);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // JP HL
        0xE9 => {
            let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
            let hl16 = builder.ins().ireduce(I16, hl);
            builder.def_var(pc_var, hl16);
            add_cycles(builder, cycles_var, CYCLE_COST[0xE9]);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // JR n
        0x18 => {
            let offset = mem[(pc + 1) as usize] as i8;
            let target = (pc as i32 + 2 + offset as i32) as u16;
            let target_val = builder.ins().iconst(I16, target as i64);
            builder.def_var(pc_var, target_val);
            add_cycles(builder, cycles_var, CYCLE_COST[0x18]);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // JR cc,n
        0x20 | 0x28 | 0x30 | 0x38 => {
            let offset = mem[(pc + 1) as usize] as i8;
            let target = (pc as i32 + 2 + offset as i32) as u16;
            let next = pc + 2;

            let (flag_bit, expect_set) = match op {
                0x20 => (flags::Z, false), // JR NZ
                0x28 => (flags::Z, true),  // JR Z
                0x30 => (flags::C, false), // JR NC
                0x38 => (flags::C, true),  // JR C
                _ => unreachable!(),
            };

            let flag_val = load_flag(builder, reg_ptr, flag_bit);
            let zero = builder.ins().iconst(I32, 0);
            let cond = if expect_set {
                builder.ins().icmp(IntCC::NotEqual, flag_val, zero)
            } else {
                builder.ins().icmp(IntCC::Equal, flag_val, zero)
            };

            let taken_block = builder.create_block();
            let not_taken_block = builder.create_block();
            builder.ins().brif(cond, taken_block, &[], not_taken_block, &[]);

            // Taken: 12 T-cycles, jump to target
            builder.switch_to_block(taken_block);
            builder.seal_block(taken_block);
            let target_val = builder.ins().iconst(I16, target as i64);
            builder.def_var(pc_var, target_val);
            add_cycles(builder, cycles_var, 12);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);

            // Not taken: 8 T-cycles, fall through
            builder.switch_to_block(not_taken_block);
            builder.seal_block(not_taken_block);
            let next_val = builder.ins().iconst(I16, next as i64);
            builder.def_var(pc_var, next_val);
            add_cycles(builder, cycles_var, 8);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // JP cc,nn
        0xC2 | 0xCA | 0xD2 | 0xDA => {
            let nn = read_u16(mem, pc + 1);
            let next = pc + 3;

            let (flag_bit, expect_set) = match op {
                0xC2 => (flags::Z, false), // JP NZ
                0xCA => (flags::Z, true),  // JP Z
                0xD2 => (flags::C, false), // JP NC
                0xDA => (flags::C, true),  // JP C
                _ => unreachable!(),
            };

            let flag_val = load_flag(builder, reg_ptr, flag_bit);
            let zero = builder.ins().iconst(I32, 0);
            let cond = if expect_set {
                builder.ins().icmp(IntCC::NotEqual, flag_val, zero)
            } else {
                builder.ins().icmp(IntCC::Equal, flag_val, zero)
            };

            let taken_block = builder.create_block();
            let not_taken_block = builder.create_block();
            builder.ins().brif(cond, taken_block, &[], not_taken_block, &[]);

            builder.switch_to_block(taken_block);
            builder.seal_block(taken_block);
            let target_val = builder.ins().iconst(I16, nn as i64);
            builder.def_var(pc_var, target_val);
            add_cycles(builder, cycles_var, 16);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);

            builder.switch_to_block(not_taken_block);
            builder.seal_block(not_taken_block);
            let next_val = builder.ins().iconst(I16, next as i64);
            builder.def_var(pc_var, next_val);
            add_cycles(builder, cycles_var, 12);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // CALL nn
        0xCD => {
            let nn = read_u16(mem, pc + 1);
            let ret_addr = pc + 3;
            // Push return address
            let sp = builder.use_var(sp_var);
            let one = builder.ins().iconst(I16, 1);
            let sp_m1 = builder.ins().isub(sp, one);
            let sp_m2 = builder.ins().isub(sp_m1, one);
            builder.def_var(sp_var, sp_m2);

            let ret_hi = builder.ins().iconst(I8, ((ret_addr >> 8) & 0xFF) as i64);
            let ret_lo = builder.ins().iconst(I8, (ret_addr & 0xFF) as i64);
            let sp_m1_32 = builder.ins().uextend(I32, sp_m1);
            emit_mem_write(builder, mem_ptr, sp_m1_32, ret_hi, state_ptr_var, cycles_var, pc_var, sp_var);
            let sp_m2_32 = builder.ins().uextend(I32, sp_m2);
            emit_mem_write(builder, mem_ptr, sp_m2_32, ret_lo, state_ptr_var, cycles_var, pc_var, sp_var);

            let target = builder.ins().iconst(I16, nn as i64);
            builder.def_var(pc_var, target);
            add_cycles(builder, cycles_var, CYCLE_COST[0xCD]);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // CALL cc,nn
        0xC4 | 0xCC | 0xD4 | 0xDC => {
            let nn = read_u16(mem, pc + 1);
            let ret_addr = pc + 3;

            let (flag_bit, expect_set) = match op {
                0xC4 => (flags::Z, false),
                0xCC => (flags::Z, true),
                0xD4 => (flags::C, false),
                0xDC => (flags::C, true),
                _ => unreachable!(),
            };

            let flag_val = load_flag(builder, reg_ptr, flag_bit);
            let zero = builder.ins().iconst(I32, 0);
            let cond = if expect_set {
                builder.ins().icmp(IntCC::NotEqual, flag_val, zero)
            } else {
                builder.ins().icmp(IntCC::Equal, flag_val, zero)
            };

            let taken_block = builder.create_block();
            let not_taken_block = builder.create_block();
            builder.ins().brif(cond, taken_block, &[], not_taken_block, &[]);

            // Taken: push return address and jump
            builder.switch_to_block(taken_block);
            builder.seal_block(taken_block);
            let sp = builder.use_var(sp_var);
            let one = builder.ins().iconst(I16, 1);
            let sp_m1 = builder.ins().isub(sp, one);
            let sp_m2 = builder.ins().isub(sp_m1, one);
            builder.def_var(sp_var, sp_m2);
            let ret_hi = builder.ins().iconst(I8, ((ret_addr >> 8) & 0xFF) as i64);
            let ret_lo = builder.ins().iconst(I8, (ret_addr & 0xFF) as i64);
            let sp_m1_32 = builder.ins().uextend(I32, sp_m1);
            emit_mem_write(builder, mem_ptr, sp_m1_32, ret_hi, state_ptr_var, cycles_var, pc_var, sp_var);
            let sp_m2_32 = builder.ins().uextend(I32, sp_m2);
            emit_mem_write(builder, mem_ptr, sp_m2_32, ret_lo, state_ptr_var, cycles_var, pc_var, sp_var);
            let target = builder.ins().iconst(I16, nn as i64);
            builder.def_var(pc_var, target);
            add_cycles(builder, cycles_var, 24);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);

            // Not taken
            builder.switch_to_block(not_taken_block);
            builder.seal_block(not_taken_block);
            let next_val = builder.ins().iconst(I16, ret_addr as i64);
            builder.def_var(pc_var, next_val);
            add_cycles(builder, cycles_var, 12);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // RET
        0xC9 => {
            emit_ret(builder, mem_ptr, sp_var, pc_var, state_ptr_var, cycles_var);
            add_cycles(builder, cycles_var, CYCLE_COST[0xC9]);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // RETI
        0xD9 => {
            emit_ret(builder, mem_ptr, sp_var, pc_var, state_ptr_var, cycles_var);
            // Enable interrupts
            let state_ptr = builder.use_var(state_ptr_var);
            let one = builder.ins().iconst(I8, 1);
            builder.ins().store(MemFlags::trusted(), one, state_ptr, off::IME);
            add_cycles(builder, cycles_var, CYCLE_COST[0xD9]);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // RET cc
        0xC0 | 0xC8 | 0xD0 | 0xD8 => {
            let (flag_bit, expect_set) = match op {
                0xC0 => (flags::Z, false),
                0xC8 => (flags::Z, true),
                0xD0 => (flags::C, false),
                0xD8 => (flags::C, true),
                _ => unreachable!(),
            };

            let flag_val = load_flag(builder, reg_ptr, flag_bit);
            let zero = builder.ins().iconst(I32, 0);
            let cond = if expect_set {
                builder.ins().icmp(IntCC::NotEqual, flag_val, zero)
            } else {
                builder.ins().icmp(IntCC::Equal, flag_val, zero)
            };

            let taken_block = builder.create_block();
            let not_taken_block = builder.create_block();
            builder.ins().brif(cond, taken_block, &[], not_taken_block, &[]);

            builder.switch_to_block(taken_block);
            builder.seal_block(taken_block);
            emit_ret(builder, mem_ptr, sp_var, pc_var, state_ptr_var, cycles_var);
            add_cycles(builder, cycles_var, 20);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);

            builder.switch_to_block(not_taken_block);
            builder.seal_block(not_taken_block);
            let next = builder.ins().iconst(I16, (pc + 1) as i64);
            builder.def_var(pc_var, next);
            add_cycles(builder, cycles_var, 8);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // RST n
        0xC7 | 0xCF | 0xD7 | 0xDF | 0xE7 | 0xEF | 0xF7 | 0xFF => {
            let target: u16 = match op {
                0xC7 => 0x00, 0xCF => 0x08, 0xD7 => 0x10, 0xDF => 0x18,
                0xE7 => 0x20, 0xEF => 0x28, 0xF7 => 0x30, 0xFF => 0x38,
                _ => unreachable!(),
            };
            let ret_addr = pc + 1;
            // Push return address
            let sp = builder.use_var(sp_var);
            let one = builder.ins().iconst(I16, 1);
            let sp_m1 = builder.ins().isub(sp, one);
            let sp_m2 = builder.ins().isub(sp_m1, one);
            builder.def_var(sp_var, sp_m2);
            let ret_hi = builder.ins().iconst(I8, ((ret_addr >> 8) & 0xFF) as i64);
            let ret_lo = builder.ins().iconst(I8, (ret_addr & 0xFF) as i64);
            let sp_m1_32 = builder.ins().uextend(I32, sp_m1);
            emit_mem_write(builder, mem_ptr, sp_m1_32, ret_hi, state_ptr_var, cycles_var, pc_var, sp_var);
            let sp_m2_32 = builder.ins().uextend(I32, sp_m2);
            emit_mem_write(builder, mem_ptr, sp_m2_32, ret_lo, state_ptr_var, cycles_var, pc_var, sp_var);
            let target_val = builder.ins().iconst(I16, target as i64);
            builder.def_var(pc_var, target_val);
            add_cycles(builder, cycles_var, CYCLE_COST[op as usize]);
            emit_exit(builder, ExitReason::BlockEnd, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // ===== STOP =====
        0x10 => {
            let next_pc = builder.ins().iconst(I16, (pc + 2) as i64);
            builder.def_var(pc_var, next_pc);
            add_cycles(builder, cycles_var, 4);
            emit_exit(builder, ExitReason::Halt, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }

        // ===== CB prefix =====
        0xCB => {
            let cb_op = mem[(pc + 1) as usize];
            emit_cb_instruction(builder, reg_ptr, mem_ptr, cb_op,
                state_ptr_var, cycles_var, pc_var, sp_var);
            // Add CB cycle cost instead of the base opcode cost
            // (the caller adds CYCLE_COST[0xCB]=0, we add the CB cost here)
            add_cycles(builder, cycles_var, CB_CYCLE_COST[cb_op as usize]);
            false
        }

        // ===== Unhandled opcodes — exit back to interpreter =====
        _ => {
            // For any opcode we haven't implemented, exit so the interpreter handles it
            let pc_val = builder.ins().iconst(I16, pc as i64);
            builder.def_var(pc_var, pc_val);
            emit_exit(builder, ExitReason::NotCompiled, state_ptr_var, cycles_var, pc_var, sp_var);
            true
        }
    }
}

/// Emit an ALU operation: ADD/ADC/SUB/SBC/AND/XOR/OR/CP
fn emit_alu_op(builder: &mut FunctionBuilder, reg_ptr: Value, alu_op: u8, src: Value) {
    let a = load_reg(builder, reg_ptr, reg::A);
    let a32 = builder.ins().uextend(I32, a);
    let src32 = builder.ins().uextend(I32, src);

    match alu_op {
        0 => { // ADD
            let result32 = builder.ins().iadd(a32, src32);
            let result = builder.ins().ireduce(I8, result32);
            store_reg(builder, reg_ptr, reg::A, result);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let a_low = builder.ins().band_imm(a32, 0xF);
            let s_low = builder.ins().band_imm(src32, 0xF);
            let low_sum = builder.ins().iadd(a_low, s_low);
            let hf = builder.ins().iconst(I32, 0xF);
            let h_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, low_sum, hf);
            let h = h_cmp;
            let cf = builder.ins().iconst(I32, 0xFF);
            let c_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, result32, cf);
            let c = c_cmp;
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        1 => { // ADC
            let carry = load_flag(builder, reg_ptr, flags::C);
            let sum1 = builder.ins().iadd(a32, src32);
            let result32 = builder.ins().iadd(sum1, carry);
            let result = builder.ins().ireduce(I8, result32);
            store_reg(builder, reg_ptr, reg::A, result);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let a_low = builder.ins().band_imm(a32, 0xF);
            let s_low = builder.ins().band_imm(src32, 0xF);
            let low_sum = builder.ins().iadd(a_low, s_low);
            let low_sum = builder.ins().iadd(low_sum, carry);
            let hf = builder.ins().iconst(I32, 0xF);
            let h_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, low_sum, hf);
            let h = h_cmp;
            let cf = builder.ins().iconst(I32, 0xFF);
            let c_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, result32, cf);
            let c = c_cmp;
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        2 => { // SUB
            let result32 = builder.ins().isub(a32, src32);
            let result = builder.ins().ireduce(I8, result32);
            store_reg(builder, reg_ptr, reg::A, result);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 1);
            let a_low = builder.ins().band_imm(a32, 0xF);
            let s_low = builder.ins().band_imm(src32, 0xF);
            let h_cmp = builder.ins().icmp(IntCC::UnsignedLessThan, a_low, s_low);
            let h = h_cmp;
            let c_cmp = builder.ins().icmp(IntCC::UnsignedLessThan, a32, src32);
            let c = c_cmp;
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        3 => { // SBC
            let carry = load_flag(builder, reg_ptr, flags::C);
            let sub1 = builder.ins().isub(a32, src32);
            let result32 = builder.ins().isub(sub1, carry);
            let result = builder.ins().ireduce(I8, result32);
            store_reg(builder, reg_ptr, reg::A, result);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 1);
            let a_low = builder.ins().band_imm(a32, 0xF);
            let s_low = builder.ins().band_imm(src32, 0xF);
            let low_sub = builder.ins().iadd(s_low, carry);
            let h_cmp = builder.ins().icmp(IntCC::UnsignedLessThan, a_low, low_sub);
            let h = h_cmp;
            let full_sub = builder.ins().iadd(src32, carry);
            let c_cmp = builder.ins().icmp(IntCC::UnsignedLessThan, a32, full_sub);
            let c = c_cmp;
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        4 => { // AND
            let result = builder.ins().band(a, src);
            store_reg(builder, reg_ptr, reg::A, result);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 1);
            let c = builder.ins().iconst(I8, 0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        5 => { // XOR
            let result = builder.ins().bxor(a, src);
            store_reg(builder, reg_ptr, reg::A, result);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().iconst(I8, 0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        6 => { // OR
            let result = builder.ins().bor(a, src);
            store_reg(builder, reg_ptr, reg::A, result);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().iconst(I8, 0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        7 => { // CP (compare — same as SUB but don't store result)
            let result32 = builder.ins().isub(a32, src32);
            let result = builder.ins().ireduce(I8, result32);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 1);
            let a_low = builder.ins().band_imm(a32, 0xF);
            let s_low = builder.ins().band_imm(src32, 0xF);
            let h_cmp = builder.ins().icmp(IntCC::UnsignedLessThan, a_low, s_low);
            let h = h_cmp;
            let c_cmp = builder.ins().icmp(IntCC::UnsignedLessThan, a32, src32);
            let c = c_cmp;
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        _ => unreachable!(),
    }
}

/// Emit DAA (Decimal Adjust Accumulator) — complex flag-dependent operation
fn emit_daa(builder: &mut FunctionBuilder, reg_ptr: Value) {
    let a = load_reg(builder, reg_ptr, reg::A);
    let a32 = builder.ins().uextend(I32, a);

    let n_flag = load_flag(builder, reg_ptr, flags::N);
    let h_flag = load_flag(builder, reg_ptr, flags::H);
    let c_flag = load_flag(builder, reg_ptr, flags::C);

    // DAA is complex — we compute it with conditional adjustments.
    // After ADD/ADC: if lower nibble > 9 or H, add 0x06. If upper > 9 or C, add 0x60.
    // After SUB/SBC: if H, subtract 0x06. If C, subtract 0x60.

    let zero32 = builder.ins().iconst(I32, 0);
    let is_sub = builder.ins().icmp(IntCC::NotEqual, n_flag, zero32);

    // Not-subtract path
    let add_block = builder.create_block();
    let sub_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, I32); // adjusted value
    builder.append_block_param(merge_block, I32); // new carry

    builder.ins().brif(is_sub, sub_block, &[], add_block, &[]);

    // ADD path
    builder.switch_to_block(add_block);
    builder.seal_block(add_block);
    let low_nib = builder.ins().band_imm(a32, 0x0F);
    let nine = builder.ins().iconst(I32, 9);
    let low_gt_9 = builder.ins().icmp(IntCC::UnsignedGreaterThan, low_nib, nine);
    let h_set = builder.ins().icmp(IntCC::NotEqual, h_flag, zero32);
    let need_low_adj = builder.ins().bor(low_gt_9, h_set);

    let adj_lo_block = builder.create_block();
    let no_adj_lo_block = builder.create_block();
    let after_lo_block = builder.create_block();
    builder.append_block_param(after_lo_block, I32); // value after low adjust

    builder.ins().brif(need_low_adj, adj_lo_block, &[], no_adj_lo_block, &[]);

    builder.switch_to_block(adj_lo_block);
    builder.seal_block(adj_lo_block);
    let six = builder.ins().iconst(I32, 6);
    let a_adj_lo = builder.ins().iadd(a32, six);
    builder.ins().jump(after_lo_block, &[a_adj_lo]);

    builder.switch_to_block(no_adj_lo_block);
    builder.seal_block(no_adj_lo_block);
    builder.ins().jump(after_lo_block, &[a32]);

    builder.switch_to_block(after_lo_block);
    builder.seal_block(after_lo_block);
    let a_after_lo = builder.block_params(after_lo_block)[0];

    // Check high nibble
    let c_set = builder.ins().icmp(IntCC::NotEqual, c_flag, zero32);
    let threshold_9f = builder.ins().iconst(I32, 0x9F);
    let high_gt_9 = builder.ins().icmp(IntCC::UnsignedGreaterThan, a_after_lo, threshold_9f);
    let need_high_adj = builder.ins().bor(high_gt_9, c_set);

    let adj_hi_block = builder.create_block();
    let no_adj_hi_block = builder.create_block();

    builder.ins().brif(need_high_adj, adj_hi_block, &[], no_adj_hi_block, &[]);

    builder.switch_to_block(adj_hi_block);
    builder.seal_block(adj_hi_block);
    let sixty = builder.ins().iconst(I32, 0x60);
    let a_adj_hi = builder.ins().iadd(a_after_lo, sixty);
    let one = builder.ins().iconst(I32, 1);
    builder.ins().jump(merge_block, &[a_adj_hi, one]);

    builder.switch_to_block(no_adj_hi_block);
    builder.seal_block(no_adj_hi_block);
    let carry_val = builder.ins().uextend(I32, c_flag);
    builder.ins().jump(merge_block, &[a_after_lo, carry_val]);

    // SUB path
    builder.switch_to_block(sub_block);
    builder.seal_block(sub_block);
    let h_set2 = builder.ins().icmp(IntCC::NotEqual, h_flag, zero32);
    let c_set2 = builder.ins().icmp(IntCC::NotEqual, c_flag, zero32);

    let sub_adj_lo_block = builder.create_block();
    let sub_no_adj_lo_block = builder.create_block();
    let sub_after_lo_block = builder.create_block();
    builder.append_block_param(sub_after_lo_block, I32);

    builder.ins().brif(h_set2, sub_adj_lo_block, &[], sub_no_adj_lo_block, &[]);

    builder.switch_to_block(sub_adj_lo_block);
    builder.seal_block(sub_adj_lo_block);
    let six2 = builder.ins().iconst(I32, 6);
    let a_sub_lo = builder.ins().isub(a32, six2);
    builder.ins().jump(sub_after_lo_block, &[a_sub_lo]);

    builder.switch_to_block(sub_no_adj_lo_block);
    builder.seal_block(sub_no_adj_lo_block);
    builder.ins().jump(sub_after_lo_block, &[a32]);

    builder.switch_to_block(sub_after_lo_block);
    builder.seal_block(sub_after_lo_block);
    let a_sub_after_lo = builder.block_params(sub_after_lo_block)[0];

    let sub_adj_hi_block = builder.create_block();
    let sub_no_adj_hi_block = builder.create_block();

    builder.ins().brif(c_set2, sub_adj_hi_block, &[], sub_no_adj_hi_block, &[]);

    builder.switch_to_block(sub_adj_hi_block);
    builder.seal_block(sub_adj_hi_block);
    let sixty2 = builder.ins().iconst(I32, 0x60);
    let a_sub_hi = builder.ins().isub(a_sub_after_lo, sixty2);
    let one2 = builder.ins().iconst(I32, 1);
    builder.ins().jump(merge_block, &[a_sub_hi, one2]);

    builder.switch_to_block(sub_no_adj_hi_block);
    builder.seal_block(sub_no_adj_hi_block);
    let carry_val2 = builder.ins().uextend(I32, c_flag);
    builder.ins().jump(merge_block, &[a_sub_after_lo, carry_val2]);

    // Merge
    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    let final_val = builder.block_params(merge_block)[0];
    let final_carry = builder.block_params(merge_block)[1];

    let result = builder.ins().band_imm(final_val, 0xFF);
    let result8 = builder.ins().ireduce(I8, result);
    store_reg(builder, reg_ptr, reg::A, result8);

    let z = compute_z(builder, result8);
    // N is preserved
    let n_val = builder.ins().ireduce(I8, n_flag);
    let h = builder.ins().iconst(I8, 0); // H always 0 after DAA
    let c = builder.ins().ireduce(I8, final_carry);
    let flags = build_flags(builder, z, n_val, h, c);
    store_reg(builder, reg_ptr, reg::F, flags);
}

/// Emit ADD HL,rr
fn emit_add_hl_rr(builder: &mut FunctionBuilder, reg_ptr: Value, hi: u8, lo: u8) {
    let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
    let rr = load_reg_pair(builder, reg_ptr, hi, lo);
    let result = builder.ins().iadd(hl, rr);
    store_reg_pair(builder, reg_ptr, reg::H, reg::L, result);

    let old_f = load_reg(builder, reg_ptr, reg::F);
    let old_f32 = builder.ins().uextend(I32, old_f);
    let z_bit = builder.ins().ushr_imm(old_f32, 7);
    let z = builder.ins().ireduce(I8, z_bit);
    let z = builder.ins().band_imm(z, 1);
    let n = builder.ins().iconst(I8, 0);
    let hl12 = builder.ins().band_imm(hl, 0xFFF);
    let rr12 = builder.ins().band_imm(rr, 0xFFF);
    let low12_sum = builder.ins().iadd(hl12, rr12);
    let hf_thr = builder.ins().iconst(I32, 0xFFF);
    let h_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, low12_sum, hf_thr);
    let h = h_cmp;
    let cf_thr = builder.ins().iconst(I32, 0xFFFF);
    let c_cmp = builder.ins().icmp(IntCC::UnsignedGreaterThan, result, cf_thr);
    let c = c_cmp;
    let flags = build_flags(builder, z, n, h, c);
    store_reg(builder, reg_ptr, reg::F, flags);
}

/// Emit PUSH rr: SP -= 2, mem[SP+1] = hi, mem[SP] = lo
fn emit_push(
    builder: &mut FunctionBuilder,
    reg_ptr: Value, hi: u8, lo: u8,
    mem_ptr: Value,
    sp_var: Variable,
    state_ptr_var: Variable,
    cycles_var: Variable,
    pc_var: Variable,
) {
    let hi_val = load_reg(builder, reg_ptr, hi);
    let lo_val = load_reg(builder, reg_ptr, lo);
    let sp = builder.use_var(sp_var);
    let one = builder.ins().iconst(I16, 1);
    let sp_m1 = builder.ins().isub(sp, one);
    let sp_m2 = builder.ins().isub(sp_m1, one);
    builder.def_var(sp_var, sp_m2);

    let sp_m1_32 = builder.ins().uextend(I32, sp_m1);
    emit_mem_write(builder, mem_ptr, sp_m1_32, hi_val, state_ptr_var, cycles_var, pc_var, sp_var);
    let sp_m2_32 = builder.ins().uextend(I32, sp_m2);
    emit_mem_write(builder, mem_ptr, sp_m2_32, lo_val, state_ptr_var, cycles_var, pc_var, sp_var);
}

/// Emit POP rr: lo = mem[SP], hi = mem[SP+1], SP += 2
fn emit_pop(
    builder: &mut FunctionBuilder,
    reg_ptr: Value, hi: u8, lo: u8,
    mem_ptr: Value,
    sp_var: Variable,
    state_ptr_var: Variable,
    cycles_var: Variable,
    pc_var: Variable,
) {
    let sp = builder.use_var(sp_var);
    let sp32 = builder.ins().uextend(I32, sp);
    let lo_val = emit_mem_read(builder, mem_ptr, sp32, state_ptr_var, cycles_var, pc_var, sp_var);
    store_reg(builder, reg_ptr, lo, lo_val);

    let one = builder.ins().iconst(I16, 1);
    let sp_p1 = builder.ins().iadd(sp, one);
    let sp_p1_32 = builder.ins().uextend(I32, sp_p1);
    let hi_val = emit_mem_read(builder, mem_ptr, sp_p1_32, state_ptr_var, cycles_var, pc_var, sp_var);
    store_reg(builder, reg_ptr, hi, hi_val);

    let two = builder.ins().iconst(I16, 2);
    let sp_new = builder.ins().iadd(sp, two);
    builder.def_var(sp_var, sp_new);
}

/// Emit RET: pop PC from stack
fn emit_ret(
    builder: &mut FunctionBuilder,
    mem_ptr: Value,
    sp_var: Variable,
    pc_var: Variable,
    state_ptr_var: Variable,
    cycles_var: Variable,
) {
    let sp = builder.use_var(sp_var);
    let sp32 = builder.ins().uextend(I32, sp);
    let lo = emit_mem_read(builder, mem_ptr, sp32, state_ptr_var, cycles_var, pc_var, sp_var);

    let one = builder.ins().iconst(I16, 1);
    let sp_p1 = builder.ins().iadd(sp, one);
    let sp_p1_32 = builder.ins().uextend(I32, sp_p1);
    let hi = emit_mem_read(builder, mem_ptr, sp_p1_32, state_ptr_var, cycles_var, pc_var, sp_var);

    let two = builder.ins().iconst(I16, 2);
    let sp_new = builder.ins().iadd(sp, two);
    builder.def_var(sp_var, sp_new);

    let hi16 = builder.ins().uextend(I16, hi);
    let lo16 = builder.ins().uextend(I16, lo);
    let hi_shifted = builder.ins().ishl_imm(hi16, 8);
    let new_pc = builder.ins().bor(hi_shifted, lo16);
    builder.def_var(pc_var, new_pc);
}

/// Emit CB-prefixed instruction
fn emit_cb_instruction(
    builder: &mut FunctionBuilder,
    reg_ptr: Value,
    mem_ptr: Value,
    cb_op: u8,
    state_ptr_var: Variable,
    cycles_var: Variable,
    pc_var: Variable,
    sp_var: Variable,
) {
    let r_idx = cb_op & 7;
    let operation = (cb_op >> 3) & 0x1F; // 5 bits: operation type + bit number

    // Load operand
    let operand = if let Some(r) = opcode_reg(r_idx) {
        load_reg(builder, reg_ptr, r)
    } else {
        // (HL)
        let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
        emit_mem_read(builder, mem_ptr, hl, state_ptr_var, cycles_var, pc_var, sp_var)
    };

    let operand32 = builder.ins().uextend(I32, operand);

    match operation {
        0 => { // RLC
            let bit7 = builder.ins().ushr_imm(operand32, 7);
            let shifted = builder.ins().ishl_imm(operand32, 1);
            let result32 = builder.ins().bor(shifted, bit7);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit7);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        1 => { // RRC
            let bit0 = builder.ins().band_imm(operand32, 1);
            let shifted = builder.ins().ushr_imm(operand32, 1);
            let bit0_to_7 = builder.ins().ishl_imm(bit0, 7);
            let result32 = builder.ins().bor(shifted, bit0_to_7);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        2 => { // RL
            let old_carry = load_flag(builder, reg_ptr, flags::C);
            let bit7 = builder.ins().ushr_imm(operand32, 7);
            let shifted = builder.ins().ishl_imm(operand32, 1);
            let result32 = builder.ins().bor(shifted, old_carry);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit7);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        3 => { // RR
            let old_carry = load_flag(builder, reg_ptr, flags::C);
            let bit0 = builder.ins().band_imm(operand32, 1);
            let shifted = builder.ins().ushr_imm(operand32, 1);
            let carry_to_7 = builder.ins().ishl_imm(old_carry, 7);
            let result32 = builder.ins().bor(shifted, carry_to_7);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        4 => { // SLA
            let bit7 = builder.ins().ushr_imm(operand32, 7);
            let result32 = builder.ins().ishl_imm(operand32, 1);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit7);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        5 => { // SRA
            let bit0 = builder.ins().band_imm(operand32, 1);
            let bit7 = builder.ins().band_imm(operand32, 0x80);
            let shifted = builder.ins().ushr_imm(operand32, 1);
            let result32 = builder.ins().bor(shifted, bit7);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        6 => { // SWAP
            let hi_nib = builder.ins().ushr_imm(operand32, 4);
            let lo_nib = builder.ins().band_imm(operand32, 0x0F);
            let lo_shifted = builder.ins().ishl_imm(lo_nib, 4);
            let result32 = builder.ins().bor(hi_nib, lo_shifted);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().iconst(I8, 0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        7 => { // SRL
            let bit0 = builder.ins().band_imm(operand32, 1);
            let result32 = builder.ins().ushr_imm(operand32, 1);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
            let z = compute_z(builder, result);
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 0);
            let c = builder.ins().ireduce(I8, bit0);
            let flags = build_flags(builder, z, n, h, c);
            store_reg(builder, reg_ptr, reg::F, flags);
        }
        8..=15 => { // BIT n,r
            let bit_num = (operation - 8) as i64;
            let mask = builder.ins().iconst(I32, 1 << bit_num);
            let result = builder.ins().band(operand32, mask);
            let zero32 = builder.ins().iconst(I32, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, result, zero32);
            let z = is_zero;
            let n = builder.ins().iconst(I8, 0);
            let h = builder.ins().iconst(I8, 1);
            // C unchanged
            let old_f = load_reg(builder, reg_ptr, reg::F);
            let old_c = builder.ins().ushr_imm(old_f, 4);
            let old_c = builder.ins().band_imm(old_c, 1);
            let flags = build_flags(builder, z, n, h, old_c);
            store_reg(builder, reg_ptr, reg::F, flags);
            // BIT doesn't write back the operand
        }
        16..=23 => { // RES n,r
            let bit_num = (operation - 16) as i64;
            let mask = builder.ins().iconst(I32, !(1i64 << bit_num));
            let result32 = builder.ins().band(operand32, mask);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
        }
        24..=31 => { // SET n,r
            let bit_num = (operation - 24) as i64;
            let mask = builder.ins().iconst(I32, 1 << bit_num);
            let result32 = builder.ins().bor(operand32, mask);
            let result = builder.ins().ireduce(I8, result32);
            store_operand(builder, reg_ptr, mem_ptr, r_idx, result, state_ptr_var, cycles_var, pc_var, sp_var);
        }
        _ => unreachable!(),
    }
}

/// Store result back to register or (HL)
fn store_operand(
    builder: &mut FunctionBuilder,
    reg_ptr: Value,
    mem_ptr: Value,
    r_idx: u8,
    val: Value,
    state_ptr_var: Variable,
    cycles_var: Variable,
    pc_var: Variable,
    sp_var: Variable,
) {
    if let Some(r) = opcode_reg(r_idx) {
        store_reg(builder, reg_ptr, r, val);
    } else {
        let hl = load_reg_pair(builder, reg_ptr, reg::H, reg::L);
        emit_mem_write(builder, mem_ptr, hl, val, state_ptr_var, cycles_var, pc_var, sp_var);
    }
}

/// Read a little-endian u16 from the memory slice
fn read_u16(mem: &[u8], addr: u16) -> u16 {
    let lo = mem[addr as usize] as u16;
    let hi = mem[(addr + 1) as usize] as u16;
    (hi << 8) | lo
}
