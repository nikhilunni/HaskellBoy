{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.Env
  ( EmulatorEnv
  , StaticRep(EmulatorEnv)
  , GBC
  , GBCEffs
  , getMemory
  , withMemory
  ) where

import Effectful
import Effectful.Dispatch.Static

import Control.Monad.ST (RealWorld)

import Effects.MemoryBus (MemoryBus)
import Effects.Trace (Trace)
import Effects.Display (Display)
import Effects.AudioOut (AudioOut)
import Effects.Input (Input)
import Types (Memory)

type GBCEffs = '[MemoryBus, Trace, Input, Display, AudioOut, EmulatorEnv, IOE]
type GBC a = Eff GBCEffs a

data EmulatorEnv :: Effect
type instance DispatchOf EmulatorEnv = 'Static 'WithSideEffects
newtype instance StaticRep EmulatorEnv = EmulatorEnv (Memory RealWorld)

getMemory :: EmulatorEnv :> es => Eff es (Memory RealWorld)
getMemory = do
  EmulatorEnv mem <- getStaticRep
  pure mem

withMemory :: EmulatorEnv :> es => (Memory RealWorld -> IO a) -> Eff es a
withMemory f = do
  EmulatorEnv mem <- getStaticRep
  unsafeEff_ (f mem)
