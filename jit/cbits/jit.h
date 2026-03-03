#ifndef HASKELLBOY_JIT_H
#define HASKELLBOY_JIT_H

#include <stdint.h>

typedef struct JITEngine JITEngine;

/* CPU state struct — pointers into Haskell's Memory arrays.
 * Layout must match Haskell Storable instance in JIT.FFI exactly. */
typedef struct {
    uint8_t* memory;       /* flat 64KB (STUArray Word16 Word8) */
    uint8_t* registers;    /* 8 bytes: A(0),F(1),B(2),C(3),D(4),E(5),H(6),L(7) */
    uint16_t pc;
    uint16_t sp;
    uint16_t cycles_used;  /* accumulated T-cycles this run */
    uint8_t  ime;          /* interrupt master enable */
    uint8_t  halt;         /* halt flag */
    uint8_t  halt_bug;     /* halt bug flag */
    uint16_t io_addr;      /* set on I/O exit: the address that triggered it */
    uint8_t  io_value;     /* for IO_WRITE: the value being written */
    uint8_t  _pad[1];      /* alignment padding */
} EmulatorState;

/* Exit reasons */
#define EXIT_BLOCK_END       0
#define EXIT_DOWNCOUNT       1
#define EXIT_IO_READ         2
#define EXIT_IO_WRITE        3
#define EXIT_HALT            4
#define EXIT_NOT_COMPILED    5

/* Lifecycle */
JITEngine* jit_engine_new(void);
void       jit_engine_free(JITEngine* engine);

/* Compile a basic block starting at `pc` from memory bytes.
 * Returns 0 on success, nonzero on error. */
int jit_compile_block(JITEngine* engine, const uint8_t* rom, uint16_t pc, uint32_t rom_len);

/* Run a compiled block. Returns exit reason. */
uint8_t jit_run_block(JITEngine* engine, EmulatorState* state, uint16_t downcount);

/* Check if a compiled block exists for this PC. Returns 1 if yes, 0 if no. */
uint8_t jit_has_block(JITEngine* engine, uint16_t pc);

/* Invalidate a compiled block (e.g., on bank switch) */
void jit_invalidate(JITEngine* engine, uint16_t pc);

/* Invalidate all compiled blocks */
void jit_invalidate_all(JITEngine* engine);

#endif /* HASKELLBOY_JIT_H */
