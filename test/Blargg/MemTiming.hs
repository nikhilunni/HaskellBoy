module Blargg.MemTiming (tests) where

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
tests = return $ testGroup "mem_timing"
  [ testGroup "mem_timing"
    [ mkRomTest "mem_timing (combined)"  "test/roms/mem_timing/mem_timing.gb"
    , mkRomTest "01-read_timing"         "test/roms/mem_timing/01-read_timing.gb"
    , mkRomTest "02-write_timing"        "test/roms/mem_timing/02-write_timing.gb"
    , mkRomTest "03-modify_timing"       "test/roms/mem_timing/03-modify_timing.gb"
    ]
  , testGroup "mem_timing-2"
    [ mkRomTest "mem_timing-2 (combined)" "test/roms/mem_timing-2/mem_timing.gb"
    , mkRomTest "01-read_timing"          "test/roms/mem_timing-2/01-read_timing.gb"
    , mkRomTest "02-write_timing"         "test/roms/mem_timing-2/02-write_timing.gb"
    , mkRomTest "03-modify_timing"        "test/roms/mem_timing-2/03-modify_timing.gb"
    ]
  ]
