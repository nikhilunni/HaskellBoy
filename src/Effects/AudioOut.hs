{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.AudioOut
  ( AudioOut(..)
  , flushAudio
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

data AudioOut :: Effect where
  FlushAudio :: AudioOut m ()

type instance DispatchOf AudioOut = 'Dynamic

flushAudio :: AudioOut :> es => Eff es ()
flushAudio = send FlushAudio
