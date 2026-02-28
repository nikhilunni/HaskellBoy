module Main where

import Data.List (isInfixOf)

import System.Directory (doesFileExist)

import Test.Tasty
import Test.Tasty.HUnit

import GBC (runRomHeadless)

romPath :: FilePath
romPath = "test/cpu_instrs.gb"

maxCycles :: Word
maxCycles = 500000000

subTests :: [(String, String)]
subTests =
  [ ("01", "01:ok")
  , ("02", "02:ok")
  , ("03", "03:ok")
  , ("04", "04:ok")
  , ("05", "05:ok")
  , ("06", "06:ok")
  , ("07", "07:ok")
  , ("08", "08:ok")
  , ("09", "09:ok")
  , ("10", "10:ok")
  , ("11", "11:ok")
  ]

mkSubTest :: String -> String -> String -> TestTree
mkSubTest label expected output =
  testCase ("sub-test " ++ label) $
    assertBool ("Expected '" ++ expected ++ "' in serial output, got:\n" ++ output)
               (expected `isInfixOf` output)

main :: IO ()
main = do
  exists <- doesFileExist romPath
  if not exists
    then putStrLn $ "ROM not found at " ++ romPath ++ ", skipping rom-tests"
    else do
      output <- runRomHeadless romPath (fromIntegral maxCycles)
      putStrLn $ "Serial output:\n" ++ output
      defaultMain $ testGroup "Blargg cpu_instrs" $
        map (\(label, expected) -> mkSubTest label expected output) subTests
