module Blargg.CpuInstrs (tests) where

import Data.List (isInfixOf)
import Data.Word (Word64)
import Control.Monad (unless)
import System.Directory (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit

import GBC (runRomHeadless)

romDir :: FilePath
romDir = "test/roms/cpu_instrs/"

cycles :: Word64
cycles = 200000000

mkRomTest :: String -> FilePath -> TestTree
mkRomTest name path = testCase name $ do
  exists <- doesFileExist path
  unless exists $ assertFailure ("ROM not found: " ++ path)
  output <- runRomHeadless path cycles
  assertBool ("Expected 'Passed' in output, got:\n" ++ output)
             ("Passed" `isInfixOf` output)

tests :: IO TestTree
tests = return $ testGroup "cpu_instrs"
  [ mkRomTest "01-special"             (romDir ++ "01-special.gb")
  , mkRomTest "02-interrupts"          (romDir ++ "02-interrupts.gb")
  , mkRomTest "03-op sp,hl"            (romDir ++ "03-op sp,hl.gb")
  , mkRomTest "04-op r,imm"            (romDir ++ "04-op r,imm.gb")
  , mkRomTest "05-op rp"               (romDir ++ "05-op rp.gb")
  , mkRomTest "06-ld r,r"              (romDir ++ "06-ld r,r.gb")
  , mkRomTest "07-jr,jp,call,ret,rst"  (romDir ++ "07-jr,jp,call,ret,rst.gb")
  , mkRomTest "08-misc instrs"         (romDir ++ "08-misc instrs.gb")
  , mkRomTest "09-op r,r"              (romDir ++ "09-op r,r.gb")
  , mkRomTest "10-bit ops"             (romDir ++ "10-bit ops.gb")
  , mkRomTest "11-op a,(hl)"           (romDir ++ "11-op a,(hl).gb")
  ]
