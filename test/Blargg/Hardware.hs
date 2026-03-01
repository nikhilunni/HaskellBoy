module Blargg.Hardware (tests) where

import Data.List (isInfixOf)
import Data.Word (Word64)
import Control.Monad (unless)
import System.Directory (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit

import GBC (runRomHeadless)

cycles :: Word64
cycles = 500000000

mkRomTest :: String -> FilePath -> TestTree
mkRomTest name path = testCase name $ do
  exists <- doesFileExist path
  unless exists $ assertFailure ("ROM not found: " ++ path)
  output <- runRomHeadless path cycles
  assertBool ("Expected 'Passed' in output, got:\n" ++ output)
             ("Passed" `isInfixOf` output)

tests :: IO TestTree
tests = return $ testGroup "hardware"
  [ testGroup "oam_bug"
    [ mkRomTest "oam_bug (combined)"   "test/roms/oam_bug/oam_bug.gb"
    , mkRomTest "1-lcd_sync"           "test/roms/oam_bug/1-lcd_sync.gb"
    , mkRomTest "2-causes"             "test/roms/oam_bug/2-causes.gb"
    , mkRomTest "3-non_causes"         "test/roms/oam_bug/3-non_causes.gb"
    , mkRomTest "4-scanline_timing"    "test/roms/oam_bug/4-scanline_timing.gb"
    , mkRomTest "5-timing_bug"         "test/roms/oam_bug/5-timing_bug.gb"
    , mkRomTest "6-timing_no_bug"      "test/roms/oam_bug/6-timing_no_bug.gb"
    , mkRomTest "7-timing_effect"      "test/roms/oam_bug/7-timing_effect.gb"
    , mkRomTest "8-instr_effect"       "test/roms/oam_bug/8-instr_effect.gb"
    ]
  , testGroup "interrupt_time"
    [ mkRomTest "interrupt_time"       "test/roms/interrupt_time/interrupt_time.gb"
    ]
  , testGroup "halt_bug"
    [ mkRomTest "halt_bug"             "test/roms/halt_bug.gb"
    ]
  ]
