{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Effects.Handlers.Headless
  ( runDisplayHeadless
  , runAudioOutHeadless
  , runInputHeadless
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

import Effects.Display (Display(..))
import Effects.AudioOut (AudioOut(..))
import Effects.Input (Input(..))

runDisplayHeadless :: Eff (Display : es) a -> Eff es a
runDisplayHeadless = interpret $ \_ -> \case
  PutPixel {} -> pure ()
  PresentFrame -> pure ()
  ClearFrame -> pure ()
  CaptureFrame _ -> pure ()

runAudioOutHeadless :: Eff (AudioOut : es) a -> Eff es a
runAudioOutHeadless = interpret $ \_ -> \case
  FlushAudio -> pure ()

runInputHeadless :: Eff (Input : es) a -> Eff es a
runInputHeadless = interpret $ \_ -> \case
  PollInput -> pure ()
  CheckQuit -> pure False
