{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.CpuBackend
  ( CpuBackend(..)
  , cpuStep
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

data CpuBackend :: Effect where
  CpuStep :: CpuBackend m ()

type instance DispatchOf CpuBackend = 'Dynamic

cpuStep :: CpuBackend :> es => Eff es ()
cpuStep = send CpuStep
