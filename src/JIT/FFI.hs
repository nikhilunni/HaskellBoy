{-# LANGUAGE ForeignFunctionInterface #-}

module JIT.FFI
  ( JITEngine
  , EmulatorState(..)
  , ExitReason(..)
  , jitEngineNew
  , jitEngineFree
  , jitCompileBlock
  , jitRunBlock
  , jitHasBlock
  , jitInvalidate
  , jitInvalidateAll
  , toExitReason
  ) where

import Foreign
import Foreign.C.Types
import Data.Word

-- | Opaque JIT engine handle
data JITEngine

-- | Exit reasons from JIT-compiled blocks (matches jit.h defines)
data ExitReason
  = ExitBlockEnd      -- ^ Block ended normally
  | ExitDowncount     -- ^ Cycle budget exhausted
  | ExitIORead        -- ^ I/O register read needed
  | ExitIOWrite       -- ^ I/O register write needed
  | ExitHalt          -- ^ HALT instruction
  | ExitNotCompiled   -- ^ Block not compiled, fall back to interpreter
  deriving (Show, Eq)

toExitReason :: Word8 -> ExitReason
toExitReason 0 = ExitBlockEnd
toExitReason 1 = ExitDowncount
toExitReason 2 = ExitIORead
toExitReason 3 = ExitIOWrite
toExitReason 4 = ExitHalt
toExitReason _ = ExitNotCompiled

-- | CPU state struct passed to native code.
-- Layout must match C header jit.h and Rust state.rs exactly.
data EmulatorState = EmulatorState
  { esMemory    :: {-# UNPACK #-} !(Ptr Word8)   -- ^ Pointer to 64K memory array
  , esRegisters :: {-# UNPACK #-} !(Ptr Word8)   -- ^ Pointer to 8-byte register array
  , esPc        :: {-# UNPACK #-} !Word16         -- ^ Program counter
  , esSp        :: {-# UNPACK #-} !Word16         -- ^ Stack pointer
  , esCycles    :: {-# UNPACK #-} !Word16         -- ^ Accumulated T-cycles
  , esIME       :: {-# UNPACK #-} !Word8          -- ^ Interrupt master enable
  , esHalt      :: {-# UNPACK #-} !Word8          -- ^ Halt flag
  , esHaltBug   :: {-# UNPACK #-} !Word8          -- ^ Halt bug flag
  , esIOAddr    :: {-# UNPACK #-} !Word16         -- ^ I/O address on exit
  , esIOValue   :: {-# UNPACK #-} !Word8          -- ^ I/O value for writes
  } deriving (Show)

instance Storable EmulatorState where
  sizeOf _ = ptrSize * 2 + 2 + 2 + 2 + 1 + 1 + 1 + 2 + 1 + 1  -- with padding
    where ptrSize = sizeOf (undefined :: Ptr Word8)

  alignment _ = alignment (undefined :: Ptr Word8)

  peek p = do
    let base = castPtr p :: Ptr Word8
        ptrSz = sizeOf (undefined :: Ptr Word8)
    mem  <- peekByteOff base 0
    regs <- peekByteOff base ptrSz
    pc'  <- peekByteOff base (ptrSz * 2)
    sp'  <- peekByteOff base (ptrSz * 2 + 2)
    cyc  <- peekByteOff base (ptrSz * 2 + 4)
    ime' <- peekByteOff base (ptrSz * 2 + 6)
    hlt  <- peekByteOff base (ptrSz * 2 + 7)
    bug  <- peekByteOff base (ptrSz * 2 + 8)
    ioa  <- peekByteOff base (ptrSz * 2 + 9)
    iov  <- peekByteOff base (ptrSz * 2 + 11)
    return $ EmulatorState mem regs pc' sp' cyc ime' hlt bug ioa iov

  poke p (EmulatorState mem regs pc' sp' cyc ime' hlt bug ioa iov) = do
    let base = castPtr p :: Ptr Word8
        ptrSz = sizeOf (undefined :: Ptr Word8)
    pokeByteOff base 0            mem
    pokeByteOff base ptrSz        regs
    pokeByteOff base (ptrSz * 2)      pc'
    pokeByteOff base (ptrSz * 2 + 2)  sp'
    pokeByteOff base (ptrSz * 2 + 4)  cyc
    pokeByteOff base (ptrSz * 2 + 6)  ime'
    pokeByteOff base (ptrSz * 2 + 7)  hlt
    pokeByteOff base (ptrSz * 2 + 8)  bug
    pokeByteOff base (ptrSz * 2 + 9)  ioa
    pokeByteOff base (ptrSz * 2 + 11) iov

-- FFI imports (unsafe = no GC safepoint, keeps array pointers stable)
foreign import ccall unsafe "jit_engine_new"
  jitEngineNew :: IO (Ptr JITEngine)

foreign import ccall unsafe "jit_engine_free"
  jitEngineFree :: Ptr JITEngine -> IO ()

foreign import ccall unsafe "jit_compile_block"
  jitCompileBlock :: Ptr JITEngine -> Ptr Word8 -> Word16 -> Word32 -> IO CInt

foreign import ccall unsafe "jit_run_block"
  jitRunBlock :: Ptr JITEngine -> Ptr EmulatorState -> Word16 -> IO Word8

foreign import ccall unsafe "jit_has_block"
  jitHasBlock :: Ptr JITEngine -> Word16 -> IO Word8

foreign import ccall unsafe "jit_invalidate"
  jitInvalidate :: Ptr JITEngine -> Word16 -> IO ()

foreign import ccall unsafe "jit_invalidate_all"
  jitInvalidateAll :: Ptr JITEngine -> IO ()
