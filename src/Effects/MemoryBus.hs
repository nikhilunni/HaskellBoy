{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.MemoryBus
  ( MemoryBus(..)
  , readMem
  , writeMem
  , tickMCycle
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

import Data.Word

data MemoryBus :: Effect where
  ReadMem    :: Word16 -> MemoryBus m Word8
  WriteMem   :: Word16 -> Word8 -> MemoryBus m ()
  TickMCycle :: MemoryBus m ()

type instance DispatchOf MemoryBus = 'Dynamic

{-# INLINE readMem #-}
readMem :: MemoryBus :> es => Word16 -> Eff es Word8
readMem addr = send (ReadMem addr)

{-# INLINE writeMem #-}
writeMem :: MemoryBus :> es => Word16 -> Word8 -> Eff es ()
writeMem addr val = send (WriteMem addr val)

{-# INLINE tickMCycle #-}
tickMCycle :: MemoryBus :> es => Eff es ()
tickMCycle = send TickMCycle
