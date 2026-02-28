module MemoryRules
       ( readMem
       , writeMem ) where

 import Control.Monad (when)
 import Control.Monad.IO.Class (liftIO)

 import Data.Word
 import Data.Bits ((.&.))

 import System.IO (hPutChar, hFlush, stderr)

 import Types
 import Memory

 readMem :: Word16 -> GBC Word8
 readMem addr = do
   test <- readRef testMode
   if test
     then readArr memory addr
     else readMemMapped addr

 readMemMapped :: Word16 -> GBC Word8
 readMemMapped addr
   | addr >= 0x4000 && addr <= 0x7FFF = do  -- Switchable ROM bank
       bank <- readRef romBank
       if bank <= 1
         then readArr memory addr
         else readArr romBanks (bank, addr)
   | addr >= 0xE000 && addr <= 0xFDFF = readArr memory (addr - 0x2000) -- Echo RAM
   | addr >= 0xFEA0 && addr <= 0xFEFF = return 0x00                   -- Unusable region
   | otherwise                        = readArr memory addr

 writeMem :: Word16 -> Word8 -> GBC ()
 writeMem addr val = do
   test <- readRef testMode
   if test
     then writeArr memory addr val
     else writeMemMapped addr val

 writeMemMapped :: Word16 -> Word8 -> GBC ()
 writeMemMapped 0xFF50 val = do
   active <- readRef bootRomActive
   when (val /= 0 && active) $ do
     -- Restore cartridge bytes from cartFirstPage back to 0x0000-0x00FF
     mapM_ (\i -> do
       b <- readArr cartFirstPage i
       writeArr memory i b) [0x0000..0x00FF]
     writeRef bootRomActive False
   writeArr memory 0xFF50 val
 writeMemMapped 0xFF02 val = do
   writeArr memory 0xFF02 val
   when (val == 0x81) $ do -- Transfer start + internal clock
     ch <- readArr memory 0xFF01
     liftIO $ hPutChar stderr (toEnum $ fromIntegral ch)
     liftIO $ hFlush stderr
 writeMemMapped addr val
   -- MBC1: ROM bank select (0x2000-0x3FFF)
   | addr >= 0x2000 && addr <= 0x3FFF = do
       let bank = val .&. 0x1F
       writeRef romBank (if bank == 0 then 1 else bank)
   -- Ignore writes to ROM area
   | addr < 0x2000                    = return ()
   | addr >= 0x4000 && addr <= 0x7FFF = return ()
   -- Echo RAM
   | addr >= 0xE000 && addr <= 0xFDFF = writeArr memory (addr - 0x2000) val
   -- Unusable region
   | addr >= 0xFEA0 && addr <= 0xFEFF = return ()
   | otherwise                        = writeArr memory addr val
