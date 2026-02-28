{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.ST (RealWorld)

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bits ((.&.))
import Data.IORef
import Data.List (sort)
import Data.Word
import qualified Data.ByteString.Lazy as BL

import Numeric (showHex)

import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeBaseName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Memory
import CPU

-- | A single test vector from the sm83 test suite
data TestCase = TestCase
  { tcName    :: String
  , tcInitial :: CpuState
  , tcFinal   :: CpuState
  } deriving (Show)

data CpuState = CpuState
  { csPC  :: Word16
  , csSP  :: Word16
  , csA   :: Word8
  , csB   :: Word8
  , csC   :: Word8
  , csD   :: Word8
  , csE   :: Word8
  , csF   :: Word8
  , csH   :: Word8
  , csL   :: Word8
  , csIME :: Bool
  , csIE  :: Maybe Word8  -- IE register (0xFFFF), only in initial state
  , csRAM :: [(Word16, Word8)]
  } deriving (Show)

instance FromJSON TestCase where
  parseJSON = withObject "TestCase" $ \o ->
    TestCase <$> o .: "name"
             <*> o .: "initial"
             <*> o .: "final"

instance FromJSON CpuState where
  parseJSON = withObject "CpuState" $ \o -> do
    pc'  <- o .: "pc"
    sp'  <- o .: "sp"
    a'   <- o .: "a"
    b'   <- o .: "b"
    c'   <- o .: "c"
    d'   <- o .: "d"
    e'   <- o .: "e"
    f'   <- o .: "f"
    h'   <- o .: "h"
    l'   <- o .: "l"
    ime' <- imeFromInt <$> (o .: "ime" :: Parser Int)
    ie'  <- fmap fromIntegral <$> (o .:? "ie" :: Parser (Maybe Int))
    ram' <- mapM parseRAMEntry =<< (o .: "ram")
    return CpuState { csPC = pc', csSP = sp', csA = a', csB = b'
                    , csC = c', csD = d', csE = e', csF = f'
                    , csH = h', csL = l', csIME = ime', csIE = ie'
                    , csRAM = ram' }
    where imeFromInt 0 = False
          imeFromInt _ = True

-- Parse a [addr, value] RAM entry
parseRAMEntry :: Value -> Parser (Word16, Word8)
parseRAMEntry = withArray "ram entry" $ \arr ->
  case toList arr of
    [a, v] -> (,) <$> parseJSON a <*> parseJSON v
    _      -> fail "expected [addr, val]"
  where toList = foldr (:) []

-- | Run a GBC action on an existing Memory
runWith :: Memory RealWorld -> GBC a -> IO a
runWith mem (GBC reader) = runReaderT reader mem

-- | Set up initial CPU state from a test case
setupState :: CpuState -> GBC ()
setupState st = do
  writeRef pc  (csPC st)
  writeRef sp  (csSP st)
  writeReg A   (csA st)
  writeReg B   (csB st)
  writeReg C   (csC st)
  writeReg D   (csD st)
  writeReg E   (csE st)
  writeReg F   (csF st .&. 0xF0)  -- Lower 4 bits always 0
  writeReg H   (csH st)
  writeReg L   (csL st)
  writeRef ime (csIME st)
  -- Write IE register if provided
  case csIE st of
    Just ie -> writeArr memory 0xFFFF ie
    Nothing -> return ()
  -- Write RAM entries
  forM_ (csRAM st) $ \(addr, val) ->
    writeArr memory addr val

-- | Zero out RAM addresses touched by a test case (initial + final)
cleanupRAM :: TestCase -> GBC ()
cleanupRAM tc = do
  let addrs = map fst (csRAM (tcInitial tc)) ++ map fst (csRAM (tcFinal tc))
  forM_ addrs $ \addr -> writeArr memory addr 0

-- | Check final state, return list of mismatches
checkState :: CpuState -> GBC [String]
checkState expected = do
  errs <- sequence
    [ checkReg "A" A (csA expected)
    , checkReg "B" B (csB expected)
    , checkReg "C" C (csC expected)
    , checkReg "D" D (csD expected)
    , checkReg "E" E (csE expected)
    , checkRegF (csF expected)
    , checkReg "H" H (csH expected)
    , checkReg "L" L (csL expected)
    , checkRef "PC" pc (csPC expected)
    , checkRef "SP" sp (csSP expected)
    , checkRef "IME" ime (csIME expected)
    ]
  ramErrs <- checkRAM (csRAM expected)
  return $ concat errs ++ ramErrs
  where
    checkReg label reg exp' = do
      actual <- readReg reg
      return $ if actual == exp'
        then []
        else [label ++ ": got 0x" ++ showHex8 actual ++ ", expected 0x" ++ showHex8 exp']

    checkRegF exp' = do
      actual <- readReg F
      let act = actual .&. 0xF0
          ex  = exp' .&. 0xF0
      return $ if act == ex
        then []
        else ["F: got 0x" ++ showHex8 act ++ ", expected 0x" ++ showHex8 ex]

    checkRef label accessor exp' = do
      actual <- readRef accessor
      return $ if actual == exp'
        then []
        else [label ++ ": got " ++ show actual ++ ", expected " ++ show exp']

    checkRAM pairs = concat <$> mapM checkOne pairs
      where checkOne (addr, exp') = do
              actual <- readArr memory addr
              return $ if actual == exp'
                then []
                else ["RAM[0x" ++ showHex16 addr ++ "]: got 0x" ++ showHex8 actual
                      ++ ", expected 0x" ++ showHex8 exp']

showHex8 :: Word8 -> String
showHex8 w = let s = showHex w "" in replicate (2 - length s) '0' ++ s

showHex16 :: Word16 -> String
showHex16 w = let s = showHex w "" in replicate (4 - length s) '0' ++ s

-- | Run all test cases for a single opcode file.
--   Allocates memory once and reuses it across all 1000 tests.
runOpcodeTests :: FilePath -> Assertion
runOpcodeTests path = do
  tcs <- loadTestFile path
  when (null tcs) $ assertFailure $ "No test cases loaded from " ++ path
  mem <- Memory.newForTest
  failRef <- newIORef (0 :: Int)
  msgsRef <- newIORef ([] :: [String])
  forM_ tcs $ \tc -> do
    errs <- runWith mem $ do
      setupState (tcInitial tc)
      (instr, _cycles) <- streamNextInstruction
      executeInstruction instr
      result <- checkState (tcFinal tc)
      cleanupRAM tc
      return result
    when (not $ null errs) $ do
      modifyIORef' failRef (+1)
      n <- readIORef failRef
      when (n <= 5) $  -- Collect details for first 5 failures
        modifyIORef' msgsRef (++ [unlines $
          ("  " ++ tcName tc ++ ":") : map ("    " ++) errs])
  nFail <- readIORef failRef
  msgs <- readIORef msgsRef
  when (nFail > 0) $
    assertFailure $ show nFail ++ "/" ++ show (length tcs) ++ " tests failed.\n"
      ++ unlines msgs
      ++ if nFail > 5 then "  ... and " ++ show (nFail - 5) ++ " more\n" else ""

-- | Load test cases from a JSON file
loadTestFile :: FilePath -> IO [TestCase]
loadTestFile path = do
  bytes <- BL.readFile path
  case eitherDecode bytes of
    Left err -> do
      hPutStrLn stderr $ "Failed to parse " ++ path ++ ": " ++ err
      return []
    Right tcs -> return tcs

-- | Build test tree for a single opcode file (one test node per opcode)
opcodeTest :: FilePath -> TestTree
opcodeTest path = testCase (takeBaseName path) (runOpcodeTests path)

-- | Find the test data directory
findTestDataDir :: IO FilePath
findTestDataDir = do
  env <- lookupEnv "SM83_TEST_DATA"
  case env of
    Just d  -> return d
    Nothing -> do
      let defaultDir = "test-data/sm83/v1"
      exists <- doesDirectoryExist defaultDir
      if exists
        then return defaultDir
        else do
          hPutStrLn stderr $ "Test data not found at " ++ defaultDir
          hPutStrLn stderr "Clone it with: git clone https://github.com/SingleStepTests/sm83 test-data/sm83"
          hPutStrLn stderr "Or set SM83_TEST_DATA environment variable"
          exitFailure

main :: IO ()
main = do
  dataDir <- findTestDataDir
  files <- listDirectory dataDir
  let jsonFiles = filter (\f -> take 1 f /= "." && drop (length f - 5) f == ".json") files
      sortedFiles = sort jsonFiles
      tests = map (\f -> opcodeTest (dataDir </> f)) sortedFiles
  defaultMain $ testGroup "SM83 CPU Tests" tests
