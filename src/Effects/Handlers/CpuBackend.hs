{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Effects.Handlers.CpuBackend
  ( runInterpreter
  , runTracingInterpreter
  , runCachedInterpreter
  , runJitInterpreter
  ) where

import Data.Word
import Data.Array.IO (IOArray, IOUArray, readArray, writeArray)
import qualified Data.Array.Base as ABase
import Data.STRef (readSTRef, writeSTRef)
import Control.Monad (when, replicateM_, forM_)
import Control.Monad.ST (stToIO)
import Foreign (Ptr, alloca, peek, poke, peekByteOff, copyBytes, plusPtr)
import GHC.IO (IO(..))
import GHC.Exts (RealWorld)

import Effectful (Eff, (:>), IOE, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Dispatch.Static (getStaticRep)

import Effects.CpuBackend (CpuBackend(..))
import Effects.MemoryBus (MemoryBus, tickMCycle)
import Effects.Trace (Trace, traceInstr)
import Effects.Env (EmulatorEnv, StaticRep(EmulatorEnv))

import Memory (readRef, writeRef)
import Types (Memory(..), pc, halt, ime, sp, haltBug, romBank)
import CPU (CachedInstr(..), streamNextInstruction, executeInstruction)
import JIT.FFI
import JIT.Pointers (withMemoryPtr, withRegistersPtr)
import JIT.Scheduler (nextEventCycles, batchTick)

-- Interpreter: plain fetch-decode-execute
runInterpreter :: (MemoryBus :> es, EmulatorEnv :> es, IOE :> es)
               => Eff (CpuBackend : es) a -> Eff es a
runInterpreter = interpret $ \_ -> \case
  CpuStep -> do
    (next, _) <- streamNextInstruction
    executeInstruction next

-- Tracing interpreter: logs each instruction via Trace effect
runTracingInterpreter :: (MemoryBus :> es, Trace :> es, EmulatorEnv :> es, IOE :> es)
                      => Eff (CpuBackend : es) a -> Eff es a
runTracingInterpreter = interpret $ \_ -> \case
  CpuStep -> do
    pcVal <- readRef pc
    (next, _) <- streamNextInstruction
    traceInstr pcVal (show next)
    executeInstruction next

-- Cached interpreter: caches decoded instructions per PC
runCachedInterpreter :: (MemoryBus :> es, EmulatorEnv :> es, IOE :> es)
                     => IOArray Word16 (Maybe CachedInstr)
                     -> Eff (CpuBackend : es) a -> Eff es a
runCachedInterpreter cache = interpret $ \_ -> \case
  CpuStep -> do
    pcVal <- readRef pc
    cached <- liftIO $ readArray cache pcVal
    case cached of
      Just (CachedInstr instr len) -> do
        -- Tick for fetch M-cycles (1 per byte: opcode + immediates)
        replicateM_ (fromIntegral len) tickMCycle
        writeRef pc (pcVal + len)
        -- Reproduce halt bug behavior
        bug <- readRef haltBug
        when bug $ writeRef haltBug False >> writeRef pc pcVal
        executeInstruction instr
      Nothing -> do
        -- Cold path: decode normally via streamNextInstruction
        (instr, _) <- streamNextInstruction
        pcAfter <- readRef pc
        let len = pcAfter - pcVal
        -- Cache the decoded instruction (only for fixed ROM + HRAM)
        when (pcVal < 0x4000 || pcVal >= 0xFF80) $
          liftIO $ writeArray cache pcVal (Just (CachedInstr instr len))
        executeInstruction instr

-- | Hot threshold: number of visits before a block gets JIT-compiled
hotThreshold :: Word16
hotThreshold = 10

-- JIT interpreter: tiered dispatch with hot-counting
-- Tier 1 (cached interpreter) handles cold code.
-- Tier 2 (Cranelift JIT) handles hot code.
runJitInterpreter :: (MemoryBus :> es, EmulatorEnv :> es, IOE :> es)
                  => Ptr JITEngine
                  -> IOArray Word16 (Maybe CachedInstr)
                  -> IOUArray Word16 Word16
                  -> Eff (CpuBackend : es) a -> Eff es a
runJitInterpreter engine cache hotCounter = interpret $ \_ -> \case
  CpuStep -> do
    pcVal <- readRef pc
    bankBefore <- readRef romBank
    compiled <- liftIO $ jitHasBlock engine pcVal
    if compiled /= 0
      then jitStep engine
      else do
        -- Tier 1: cached interpreter with hot counting
        cached <- liftIO $ readArray cache pcVal
        case cached of
          Just (CachedInstr instr len) -> do
            replicateM_ (fromIntegral len) tickMCycle
            writeRef pc (pcVal + len)
            bug <- readRef haltBug
            when bug $ writeRef haltBug False >> writeRef pc pcVal
            executeInstruction instr
            -- Increment hot counter (cap at hotThreshold to prevent wrap-around)
            count <- liftIO $ readArray hotCounter pcVal
            when (count < hotThreshold) $ do
              let count' = count + 1
              liftIO $ writeArray hotCounter pcVal count'
              when (count' == hotThreshold) $
                compileBlock engine pcVal
          Nothing -> do
            (instr, _) <- streamNextInstruction
            pcAfter <- readRef pc
            let len = pcAfter - pcVal
            when (pcVal < 0x4000 || pcVal >= 0xFF80) $
              liftIO $ writeArray cache pcVal (Just (CachedInstr instr len))
            executeInstruction instr
    -- If a ROM bank switch occurred, sync memory and invalidate JIT blocks
    bankAfter <- readRef romBank
    when (bankBefore /= bankAfter) $ do
      -- Copy new bank's ROM data into memory[0x4000-0x7FFF]
      EmulatorEnv mem <- getStaticRep
      liftIO $ syncRomBank mem bankAfter
      liftIO $ jitInvalidateAll engine

-- | Execute a JIT-compiled block: sync state to FFI struct, run native code,
-- handle the exit reason, sync state back.
jitStep :: (MemoryBus :> es, EmulatorEnv :> es, IOE :> es)
        => Ptr JITEngine -> Eff es ()
jitStep engine = do
  -- Compute downcount from event scheduler
  downcount <- nextEventCycles

  -- Sync Haskell state → EmulatorState struct and run native code
  EmulatorEnv mem <- getStaticRep
  (st, exitCode) <- liftIO $ withMemoryPtr mem $ \memPtr ->
    withRegistersPtr mem $ \regPtr ->
      alloca $ \statePtr -> do
        pcVal  <- stToIO $ readSTRef (pc mem)
        spVal  <- stToIO $ readSTRef (sp mem)
        imeVal <- stToIO $ readSTRef (ime mem)
        hltVal <- stToIO $ readSTRef (halt mem)
        bugVal <- stToIO $ readSTRef (haltBug mem)
        poke statePtr $ EmulatorState
          { esMemory    = memPtr
          , esRegisters = regPtr
          , esPc        = pcVal
          , esSp        = spVal
          , esCycles    = 0
          , esIME       = if imeVal then 1 else 0
          , esHalt      = if hltVal then 1 else 0
          , esHaltBug   = if bugVal then 1 else 0
          , esIOAddr    = 0
          , esIOValue   = 0
          }
        exitCode <- jitRunBlock engine statePtr downcount
        st <- peek statePtr
        -- Write back PC, SP, flags to Haskell state
        stToIO $ writeSTRef (pc mem) (esPc st)
        stToIO $ writeSTRef (sp mem) (esSp st)
        stToIO $ writeSTRef (ime mem) (esIME st /= 0)
        stToIO $ writeSTRef (halt mem) (esHalt st /= 0)
        stToIO $ writeSTRef (haltBug mem) (esHaltBug st /= 0)
        return (st, exitCode)

  -- Batch-tick GPU/APU for the cycles consumed by native code
  let cyclesUsed = esCycles st
  when (cyclesUsed > 0) $ batchTick cyclesUsed

  -- Handle exit reason
  case toExitReason exitCode of
    ExitBlockEnd   -> return ()  -- Normal block end, continue
    ExitDowncount  -> return ()  -- Back to main loop for event processing
    ExitHalt       -> writeRef halt True
    ExitIORead     -> do
      -- If the block exited immediately (0 cycles consumed), the first
      -- instruction is I/O. Invalidate so the cached interpreter handles
      -- it directly, avoiding FFI overhead on every iteration.
      when (esCycles st == 0) $
        liftIO $ jitInvalidate engine (esPc st)
      (instr, _) <- streamNextInstruction
      executeInstruction instr
    ExitIOWrite    -> do
      when (esCycles st == 0) $
        liftIO $ jitInvalidate engine (esPc st)
      (instr, _) <- streamNextInstruction
      executeInstruction instr
      -- If the write was a ROM bank switch (0x2000-0x3FFF), invalidate
      -- all compiled blocks since banked ROM code may have changed.
      let ioAddr = esIOAddr st
      when (ioAddr >= 0x2000 && ioAddr <= 0x3FFF) $
        liftIO $ jitInvalidateAll engine
    ExitNotCompiled -> return ()

-- | Try to compile a block at the given PC.
-- Skip blocks starting with I/O instructions (LDH) — they exit immediately
-- via ExitIORead/Write, making the cached interpreter more efficient.
-- Also skip HRAM blocks (0xFF80-0xFFFE) — these are typically small delay
-- loops where interrupt timing at instruction boundaries matters. The JIT
-- checks interrupts at block boundaries, which causes re-execution of
-- instructions after interrupt return, corrupting delay loop counts.
compileBlock :: (EmulatorEnv :> es, IOE :> es)
             => Ptr JITEngine -> Word16 -> Eff es ()
compileBlock engine pcAddr = do
  -- Skip HRAM (0xFF80+) — interrupt timing is instruction-granularity there.
  -- Banked ROM (0x4000-0x7FFF) is OK: memory[] is synced on bank switch,
  -- and compiled blocks are invalidated when the bank changes.
  when (pcAddr < 0xFF80) $ do
    EmulatorEnv mem <- getStaticRep
    liftIO $ withMemoryPtr mem $ \memPtr -> do
      firstOp <- peekByteOff memPtr (fromIntegral pcAddr) :: IO Word8
      -- 0xE0=LDH(n),A  0xF0=LDH A,(n)  0xE2=LDH(C),A  0xF2=LDH A,(C)
      when (firstOp /= 0xE0 && firstOp /= 0xF0 && firstOp /= 0xE2 && firstOp /= 0xF2) $ do
        _ <- jitCompileBlock engine memPtr pcAddr 0x10000
        return ()

-- | Sync banked ROM data into memory[0x4000-0x7FFF] using pointer copy.
-- Called when a ROM bank switch is detected by the JIT handler.
syncRomBank :: Memory RealWorld -> Word8 -> IO ()
syncRomBank mem bank =
  withMemoryPtr mem $ \memPtr -> do
    let dstPtr = memPtr `plusPtr` 0x4000
    -- Read 16KB from romBanks[bank, 0x4000..0x7FFF] into memory
    -- Use individual reads since romBanks is a 2D STUArray
    forM_ [0x4000 :: Word16 .. 0x7FFF] $ \addr -> do
      val <- stToIO $ ABase.readArray (romBanks mem) (bank, addr)
      poke (dstPtr `plusPtr` fromIntegral (addr - 0x4000)) val
