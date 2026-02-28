module Cartridge
       ( readCartridge
       , storeCartridge
       , loadBootRom
       , romBankCount) where

 import qualified Data.ByteString as C

 import Data.Word

 import Types
 import Memory
 import MemoryRules

 readCartridge :: FilePath -> IO [Word8]
 readCartridge fpath = C.unpack <$> C.readFile fpath

 -- | Number of ROM banks based on header byte at 0x148
 romBankCount :: [Word8] -> Word8
 romBankCount rom
   | length rom > 0x148 = case rom !! 0x148 of
       0x00 -> 2   -- 32KB
       0x01 -> 4   -- 64KB
       0x02 -> 8   -- 128KB
       0x03 -> 16  -- 256KB
       0x04 -> 32  -- 512KB
       0x05 -> 64  -- 1MB
       0x06 -> 128 -- 2MB
       _    -> 2
   | otherwise = 2

 storeCartridge :: [Word8] -> GBC ()
 storeCartridge ws = do
   -- Write bank 0 (0x0000-0x3FFF) and bank 1 (0x4000-0x7FFF) to main memory
   let firstTwoBanks = take 0x8000 ws
   mapM_ (\(c,i) -> writeArr memory (fromIntegral i) c) $ zip firstTwoBanks [0::Int ..]
   -- Write remaining banks (2+) to romBanks array
   let remaining = drop 0x8000 ws
   mapM_ (\(c,i) -> do
     let bankNum = fromIntegral (i `div` 0x4000 + 2) :: Word8
         addr    = fromIntegral (i `mod` 0x4000) + 0x4000 :: Word16
     writeArr romBanks (bankNum, addr) c
     ) $ zip remaining [0::Int ..]

 loadBootRom :: [Word8] -> GBC ()
 loadBootRom bootRom = do
   -- Save cartridge bytes at 0x0000-0x00FF into cartFirstPage
   mapM_ (\i -> do
     val <- readMem i
     writeArr cartFirstPage i val) [0x0000..0x00FF]
   -- Write boot ROM over 0x0000-0x00FF
   mapM_ (\(b,i) -> writeMem i b) $ zip bootRom [0x0000..]
   -- Mark boot ROM as active
   writeRef bootRomActive True
