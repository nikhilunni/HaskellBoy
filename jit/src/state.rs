/// Exit reasons — must match jit.h defines
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExitReason {
    BlockEnd = 0,
    Downcount = 1,
    IoRead = 2,
    IoWrite = 3,
    Halt = 4,
    NotCompiled = 5,
}

/// Register indices in the registers array (matches Haskell Register enum order)
#[allow(dead_code)]
pub mod reg {
    pub const A: u8 = 0;
    pub const F: u8 = 1;
    pub const B: u8 = 2;
    pub const C: u8 = 3;
    pub const D: u8 = 4;
    pub const E: u8 = 5;
    pub const H: u8 = 6;
    pub const L: u8 = 7;
}

/// Flag bit positions in the F register
pub mod flags {
    pub const Z: u8 = 7; // Zero flag
    pub const N: u8 = 6; // Subtract flag
    pub const H: u8 = 5; // Half-carry flag
    pub const C: u8 = 4; // Carry flag
}

/// EmulatorState layout — must match C header and Haskell Storable instance.
/// We don't use #[repr(C)] struct directly; instead we access fields via offsets
/// from the pointer passed by Haskell.
pub mod state_offsets {
    use std::mem::size_of;

    // Pointer fields (8 bytes each on 64-bit)
    pub const MEMORY: i32 = 0;
    pub const REGISTERS: i32 = size_of::<usize>() as i32;

    // After the two pointers
    pub const PC: i32 = (2 * size_of::<usize>()) as i32;
    pub const SP: i32 = PC + 2;
    pub const CYCLES_USED: i32 = SP + 2;
    pub const IME: i32 = CYCLES_USED + 2;
    pub const HALT: i32 = IME + 1;
    pub const HALT_BUG: i32 = HALT + 1;
    pub const IO_ADDR: i32 = HALT_BUG + 1;
    pub const IO_VALUE: i32 = IO_ADDR + 2;
}

/// Check if an address is in the I/O register range that requires Haskell handling.
/// 0xFF00-0xFF7F: I/O registers (timer, LCD, sound, joypad, etc.)
/// We also exit for 0xFFFF (IE register) since it's interrupt-related.
#[inline]
pub fn is_io_addr(addr: u16) -> bool {
    (addr >= 0xFF00 && addr < 0xFF80) || addr == 0xFFFF
}

/// Check if an opcode is a block terminator (matches CPU.hs isBlockTerminator)
pub fn is_block_terminator(op: u8) -> bool {
    matches!(
        op,
        0xC3 | 0xE9 | 0x18 // JP nn, JP HL, JR n
        | 0xC2 | 0xCA | 0xD2 | 0xDA | 0x20 | 0x28 | 0x30 | 0x38 // JP/JR cc
        | 0xCD | 0xC4 | 0xCC | 0xD4 | 0xDC // CALL nn / CALL cc
        | 0xC9 | 0xD9 | 0xC0 | 0xC8 | 0xD0 | 0xD8 // RET / RETI / RET cc
        | 0xC7 | 0xCF | 0xD7 | 0xDF | 0xE7 | 0xEF | 0xF7 | 0xFF // RST
        | 0x76 | 0x10 // HALT, STOP
    )
}

/// Instruction length in bytes (matches CPU.hs lengthLookup)
pub static INSTR_LENGTH: [u8; 256] = [
    1,3,1,1,1,1,2,1,3,1,1,1,1,1,2,1, // 0x
    2,3,1,1,1,1,2,1,2,1,1,1,1,1,2,1, // 1x
    2,3,1,1,1,1,2,1,2,1,1,1,1,1,2,1, // 2x
    2,3,1,1,1,1,2,1,2,1,1,1,1,1,2,1, // 3x
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 4x
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 5x
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 6x
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 7x
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 8x
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 9x
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // Ax
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // Bx
    1,1,3,3,3,1,2,1,1,1,3,2,3,3,2,1, // Cx (CB prefix = 2 bytes)
    1,1,3,0,3,1,2,1,1,1,3,0,3,0,2,1, // Dx
    2,1,1,0,0,1,2,1,2,1,3,0,0,0,2,1, // Ex
    2,1,1,1,0,1,2,1,2,1,3,1,0,0,2,1, // Fx
];

/// T-cycle cost per opcode (matches CPU.hs cycleLookup)
pub static CYCLE_COST: [u16; 256] = [
    4,12,8,8,4,4,8,4,20,8,8,8,4,4,8,4,       // 0x
    4,12,8,8,4,4,8,4,8,8,8,8,4,4,8,4,        // 1x
    8,12,8,8,4,4,8,4,8,8,8,8,4,4,8,4,        // 2x
    8,12,8,8,12,12,12,4,8,8,8,8,4,4,8,4,     // 3x
    4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,         // 4x
    4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,         // 5x
    4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,         // 6x
    8,8,8,8,8,8,4,8,4,4,4,4,4,4,8,4,         // 7x
    4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,         // 8x
    4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,         // 9x
    4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,         // Ax
    4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,         // Bx
    8,12,12,16,12,16,8,16,8,16,12,0,12,24,8,16, // Cx
    8,12,12,0,12,16,8,16,8,16,12,0,12,0,8,16,   // Dx
    12,12,8,0,0,16,8,16,16,4,16,0,0,0,8,16,     // Ex
    12,12,8,4,0,16,8,16,12,8,16,4,0,0,8,16,     // Fx
];

/// T-cycle cost for CB-prefixed instructions
pub static CB_CYCLE_COST: [u16; 256] = [
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // 0x
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // 1x
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // 2x
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // 3x
    8,8,8,8,8,8,12,8,8,8,8,8,8,8,12,8, // 4x (BIT n,(HL) = 12)
    8,8,8,8,8,8,12,8,8,8,8,8,8,8,12,8, // 5x
    8,8,8,8,8,8,12,8,8,8,8,8,8,8,12,8, // 6x
    8,8,8,8,8,8,12,8,8,8,8,8,8,8,12,8, // 7x
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // 8x
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // 9x
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // Ax
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // Bx
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // Cx
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // Dx
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // Ex
    8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8, // Fx
];
