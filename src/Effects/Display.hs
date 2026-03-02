{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Effects.Display
  ( Display(..)
  , putPixel
  , presentFrame
  , clearFrame
  , captureFrame
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

import Data.Word

data Display :: Effect where
  PutPixel     :: !Int -> !Int -> !Word8 -> !Word8 -> !Word8 -> !Word8 -> Display m ()
  PresentFrame :: Display m ()
  ClearFrame   :: Display m ()
  CaptureFrame :: FilePath -> Display m ()

type instance DispatchOf Display = 'Dynamic

putPixel :: Display :> es => Int -> Int -> Word8 -> Word8 -> Word8 -> Word8 -> Eff es ()
putPixel x y a r g b = send (PutPixel x y a r g b)

presentFrame :: Display :> es => Eff es ()
presentFrame = send PresentFrame

clearFrame :: Display :> es => Eff es ()
clearFrame = send ClearFrame

captureFrame :: Display :> es => FilePath -> Eff es ()
captureFrame path = send (CaptureFrame path)
