module Cartridge
       ( readCartridge
       , storeCartridge
       , loadBootRom
       , romBankCount) where

 import qualified Data.ByteString as C

 import Control.Monad (when)
 import Data.Word

 import Types
 import Memory
 import Effects.MemoryBus (readMem, writeMem)
 import Effects.Env (GBC)

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
   -- Also write bank 1 to romBanks (for ROM bank sync when switching back to bank 1)
   let bank1Data = take 0x4000 (drop 0x4000 ws)
   mapM_ (\(c,i) -> writeArr romBanks (1, fromIntegral i + 0x4000) c)
     $ zip bank1Data [0::Int ..]
   -- Write remaining banks (2+) to romBanks array
   let remaining = drop 0x8000 ws
   mapM_ (\(c,i) -> do
     let bankNum = fromIntegral (i `div` 0x4000 + 2) :: Word8
         addr    = fromIntegral (i `mod` 0x4000) + 0x4000 :: Word16
     writeArr romBanks (bankNum, addr) c
     ) $ zip remaining [0::Int ..]
   -- Detect CGB mode from cartridge header byte 0x143
   when (length ws > 0x143) $ do
     let cgbFlag = ws !! 0x143
     when (cgbFlag == 0x80 || cgbFlag == 0xC0) $
       writeRef gbc_mode True
   -- Detect MBC type from header byte 0x0147 and set ROM bank mask
   when (length ws > 0x147) $ do
     let mbcByte = ws !! 0x147
         mask = case mbcByte of
                  b | b >= 0x0F && b <= 0x13 -> 0x7F  -- MBC3: 7-bit bank register
                  b | b >= 0x19 && b <= 0x1E -> 0xFF  -- MBC5: 8-bit bank register
                  _                          -> 0x1F  -- MBC1/ROM only: 5-bit
     writeRef romBankMask mask

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
