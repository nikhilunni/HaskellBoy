{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Effects.Handlers.CpuBackend
  ( runInterpreter
  , runTracingInterpreter
  ) where

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)

import Effects.CpuBackend (CpuBackend(..))
import Effects.MemoryBus (MemoryBus)
import Effects.Trace (Trace, traceInstr)
import Effects.Env (EmulatorEnv)

import Effectful (IOE)
import Memory (readRef)
import Types (pc)
import CPU (streamNextInstruction, executeInstruction)

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
