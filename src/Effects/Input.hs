{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.Input
  ( Input(..)
  , pollInput
  , checkQuit
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

data Input :: Effect where
  PollInput :: Input m ()
  CheckQuit :: Input m Bool

type instance DispatchOf Input = 'Dynamic

pollInput :: Input :> es => Eff es ()
pollInput = send PollInput

checkQuit :: Input :> es => Eff es Bool
checkQuit = send CheckQuit
