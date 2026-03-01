module Blargg.InstrTiming (tests) where

import Data.List (isInfixOf)
import Data.Word (Word64)
import Control.Monad (unless)
import System.Directory (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit

import GBC (runRomHeadless)

mkRomTest :: String -> FilePath -> Word64 -> TestTree
mkRomTest name path cyc = testCase name $ do
  exists <- doesFileExist path
  unless exists $ assertFailure ("ROM not found: " ++ path)
  output <- runRomHeadless path cyc
  assertBool ("Expected 'Passed' in output, got:\n" ++ output)
             ("Passed" `isInfixOf` output)

tests :: IO TestTree
tests = return $ testGroup "instr_timing"
  [ mkRomTest "instr_timing" "test/roms/instr_timing/instr_timing.gb" 500000000
  ]
