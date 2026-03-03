{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module JIT.Scheduler
  ( nextEventCycles
  , batchTick
  ) where

import Data.Word
import Data.Bits ((.&.), testBit)
import Control.Monad (replicateM_)

import Effectful (Eff, (:>))

import Types
import Memory (readRef, readArr)
import Effects.Env (EmulatorEnv)
import Effects.MemoryBus (MemoryBus, tickMCycle)

-- | Compute how many T-cycles until the next event that needs Haskell handling.
-- This determines how long native code can run uninterrupted.
nextEventCycles :: (EmulatorEnv :> es) => Eff es Word16
nextEventCycles = do
  -- GPU: cycles until next mode transition
  gpuCyc  <- readRef gpu_cycles
  gpuMode <- readRef mode
  let gpuNext = case gpuMode of
        OAMSearch  -> max 1 (80 - gpuCyc)
        Transfer   -> max 1 (252 - gpuCyc)
        HBlankMode -> max 1 (456 - gpuCyc)
        VBlankMode -> max 1 (456 - gpuCyc)

  -- Timer: cycles until next TIMA tick
  tac <- readArr memory 0xFF07
  timerCyc <- readRef timerCounter
  let timerEnabled = testBit tac 2
      timerPeriod = case tac .&. 0x03 of
                      0 -> 1024
                      1 -> 16
                      2 -> 64
                      _ -> 256
      timerNext = if timerEnabled then max 1 (timerPeriod - timerCyc) else 0xFFFF

  -- APU frame sequencer: cycles until next step
  apuCyc <- readRef apuFrameSeqCounter
  let apuNext = max 1 (8192 - apuCyc)

  -- DIV timer: cycles until next DIV increment
  divCyc <- readRef divCounter
  let divNext = max 1 (256 - divCyc)

  -- Return the minimum, clamped to at least 4 (one M-cycle)
  return $ max 4 (minimum [gpuNext, timerNext, apuNext, divNext])

-- | Batch-tick the GPU/APU/Timer for N T-cycles by calling tickMCycle N/4 times.
-- This reuses the existing per-M-cycle ticking logic exactly.
batchTick :: (MemoryBus :> es, EmulatorEnv :> es) => Word16 -> Eff es ()
batchTick n = replicateM_ (fromIntegral n `div` 4) tickMCycle
