module Blargg.DmgSound (tests) where

import Data.List (isInfixOf)
import Data.Word (Word64)
import Control.Monad (unless)
import System.Directory (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit

import GBC (runRomHeadless)

romDir :: FilePath
romDir = "test/roms/dmg_sound/"

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
tests = return $ testGroup "dmg_sound"
  [ mkRomTest "dmg_sound (combined)"      (romDir ++ "dmg_sound.gb")
  , mkRomTest "01-registers"              (romDir ++ "01-registers.gb")
  , mkRomTest "02-len ctr"                (romDir ++ "02-len ctr.gb")
  , mkRomTest "03-trigger"                (romDir ++ "03-trigger.gb")
  , mkRomTest "04-sweep"                  (romDir ++ "04-sweep.gb")
  , mkRomTest "05-sweep details"          (romDir ++ "05-sweep details.gb")
  , mkRomTest "06-overflow on trigger"    (romDir ++ "06-overflow on trigger.gb")
  , mkRomTest "07-len sweep period sync"  (romDir ++ "07-len sweep period sync.gb")
  , mkRomTest "08-len ctr during power"   (romDir ++ "08-len ctr during power.gb")
  , mkRomTest "09-wave read while on"     (romDir ++ "09-wave read while on.gb")
  , mkRomTest "10-wave trigger while on"  (romDir ++ "10-wave trigger while on.gb")
  , mkRomTest "11-regs after power"       (romDir ++ "11-regs after power.gb")
  , mkRomTest "12-wave write while on"    (romDir ++ "12-wave write while on.gb")
  ]
