module Main where

import Test.Tasty

import qualified Blargg.CpuInstrs
import qualified Blargg.InstrTiming
import qualified Blargg.MemTiming
import qualified Blargg.DmgSound
import qualified Blargg.CgbSound
import qualified Blargg.Hardware

main :: IO ()
main = do
  cpuInstrs   <- Blargg.CpuInstrs.tests
  instrTiming <- Blargg.InstrTiming.tests
  memTiming   <- Blargg.MemTiming.tests
  dmgSound    <- Blargg.DmgSound.tests
  cgbSound    <- Blargg.CgbSound.tests
  hardware    <- Blargg.Hardware.tests
  defaultMain $ testGroup "Blargg"
    [ cpuInstrs
    , instrTiming
    , memTiming
    , dmgSound
    , cgbSound
    , hardware
    ]
