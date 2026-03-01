module MemoryRules
       ( readMem
       , writeMem
       , tickMCycle ) where

 import Control.Monad (when, unless)
 import Control.Monad.IO.Class (liftIO)

 import Control.Monad.ST (RealWorld)
 import Data.Word
 import Data.STRef
 import Data.Bits ((.&.), (.|.), testBit, shiftL, shiftR)

 import System.IO (hPutChar, hFlush, stderr)

 import Types
 import Memory
 import qualified GPU
 import qualified APU

 {-# INLINE tickMCycle #-}
 tickMCycle :: GBC ()
 tickMCycle = do
   test <- readRef testMode
   unless test $ do
     GPU.tick 4
     APU.tickAPU 4
     cnt <- readRef mCycleCounter
     writeRef mCycleCounter (cnt + 4)

 readMem :: Word16 -> GBC Word8
 readMem addr = do
   test <- readRef testMode
   if test
     then readArr memory addr
     else do
       tickMCycle
       readMemMapped addr

 readMemMapped :: Word16 -> GBC Word8
 readMemMapped addr
   | addr >= 0x8000 && addr <= 0x9FFF = do  -- VRAM: blocked during Transfer (Mode 3)
       m <- readRef mode
       case m of
         Transfer -> return 0xFF
         _        -> readArr memory addr
   | addr >= 0x4000 && addr <= 0x7FFF = do  -- Switchable ROM bank
       bank <- readRef romBank
       if bank <= 1
         then readArr memory addr
         else do
           nBanks <- readRef numRomBanks
           let clampedBank = if nBanks > 1
                               then ((bank - 1) `mod` (nBanks - 1)) + 1
                               else 1
           readArr romBanks (clampedBank, addr)
   | addr >= 0xE000 && addr <= 0xFDFF = readArr memory (addr - 0x2000) -- Echo RAM
   | addr >= 0xFE00 && addr <= 0xFE9F = do  -- OAM: blocked during OAMSearch and Transfer
       m <- readRef mode
       case m of
         OAMSearch -> return 0xFF
         Transfer  -> return 0xFF
         _         -> readArr memory addr
   | addr >= 0xFEA0 && addr <= 0xFEFF = return 0x00                   -- Unusable region
   | addr == 0xFF00                   = readJoypad                     -- Joypad register
   | addr == 0xFF26                   = readNR52                       -- NR52: build from APU state
   | addr >= 0xFF10 && addr <= 0xFF2F = readSoundReg addr             -- Sound registers
   | addr == 0xFF76                   = readPCM12                      -- PCM12: ch1/ch2 amplitude
   | addr == 0xFF77                   = readPCM34                      -- PCM34: ch3/ch4 amplitude
   | otherwise                        = readArr memory addr

 -- Joypad register (0xFF00): select via bits 4-5, read buttons/dpad in bits 0-3
 readJoypad :: GBC Word8
 readJoypad = do
   sel <- readArr memory 0xFF00
   btns <- readRef joypadButtons
   dirs <- readRef joypadDirPad
   let input = (if testBit sel 4 then 0x0F else dirs .&. 0x0F)
             .&. (if testBit sel 5 then 0x0F else btns .&. 0x0F)
   return $ 0xC0 .|. (sel .&. 0x30) .|. input

 -- NR52: bit 7 = power, bits 3-0 = channel active, bits 6-4 always read as 1
 readNR52 :: GBC Word8
 readNR52 = do
   power <- readRef apuPower
   c1 <- readRef ch1Active
   c2 <- readRef ch2Active
   c3 <- readRef ch3Active
   c4 <- readRef ch4Active
   let val = 0x70  -- Bits 6-4 always 1
           .|. (if power then 0x80 else 0x00)
           .|. (if c1    then 0x01 else 0x00)
           .|. (if c2    then 0x02 else 0x00)
           .|. (if c3    then 0x04 else 0x00)
           .|. (if c4    then 0x08 else 0x00)
   return val

 -- PCM12 (0xFF76): bits 0-3 = ch1 volume, bits 4-7 = ch2 volume
 readPCM12 :: GBC Word8
 readPCM12 = do
   v1 <- readRef ch1EnvelopeVolume
   v2 <- readRef ch2EnvelopeVolume
   return $ (v2 `shiftL` 4) .|. (v1 .&. 0x0F)

 -- PCM34 (0xFF77): bits 0-3 = ch3 volume, bits 4-7 = ch4 volume
 readPCM34 :: GBC Word8
 readPCM34 = do
   -- Ch3 volume is determined by NR32 shift, not envelope
   nr32 <- readArr memory 0xFF1C
   let v3 = case (nr32 `shiftR` 5) .&. 0x03 of
              0 -> 0   -- mute
              1 -> 15  -- 100%
              2 -> 8   -- 50%
              3 -> 4   -- 25%
              _ -> 0
   v4 <- readRef ch4EnvelopeVolume
   return $ (v4 `shiftL` 4) .|. (v3 .&. 0x0F)

 -- Sound registers have unreadable bits that always read as 1
 readSoundReg :: Word16 -> GBC Word8
 readSoundReg addr
   | addr >= 0xFF27 = return 0xFF  -- Unused sound range
   | otherwise = do
       val <- readArr memory addr
       return $ val .|. soundRegMask addr

 soundRegMask :: Word16 -> Word8
 soundRegMask 0xFF10 = 0x80
 soundRegMask 0xFF11 = 0x3F
 soundRegMask 0xFF12 = 0x00
 soundRegMask 0xFF13 = 0xFF
 soundRegMask 0xFF14 = 0xBF
 soundRegMask 0xFF15 = 0xFF
 soundRegMask 0xFF16 = 0x3F
 soundRegMask 0xFF17 = 0x00
 soundRegMask 0xFF18 = 0xFF
 soundRegMask 0xFF19 = 0xBF
 soundRegMask 0xFF1A = 0x7F
 soundRegMask 0xFF1B = 0xFF
 soundRegMask 0xFF1C = 0x9F
 soundRegMask 0xFF1D = 0xFF
 soundRegMask 0xFF1E = 0xBF
 soundRegMask 0xFF1F = 0xFF
 soundRegMask 0xFF20 = 0xFF
 soundRegMask 0xFF21 = 0x00
 soundRegMask 0xFF22 = 0x00
 soundRegMask 0xFF23 = 0xBF
 soundRegMask 0xFF24 = 0x00
 soundRegMask 0xFF25 = 0x00
 soundRegMask 0xFF26 = 0x70
 soundRegMask _      = 0xFF

 writeMem :: Word16 -> Word8 -> GBC ()
 writeMem addr val = do
   test <- readRef testMode
   if test
     then writeArr memory addr val
     else do
       tickMCycle
       writeMemMapped addr val

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
 writeMemMapped 0xFF04 _ = do -- DIV: any write resets to 0
   writeArr memory 0xFF04 0
   writeRef divCounter 0
   writeRef timerCounter 0
 writeMemMapped 0xFF02 val = do
   writeArr memory 0xFF02 val
   when (val == 0x81) $ do -- Transfer start + internal clock
     ch <- readArr memory 0xFF01
     let c = toEnum $ fromIntegral ch
     buf <- readRef serialBuffer
     writeRef serialBuffer (buf ++ [c])
     liftIO $ hPutChar stderr c
     liftIO $ hFlush stderr
 -- NR52: power control
 writeMemMapped 0xFF26 val = do
   let newPower = testBit val 7
   oldPower <- readRef apuPower
   writeRef apuPower newPower
   if not newPower && oldPower
     then do
       gbc <- readRef gbc_mode
       if gbc
         then do
           -- CGB: clear ALL sound registers FF10-FF25 including NR41
           mapM_ (\a -> writeArr memory a 0) [0xFF10..0xFF25]
           writeRef ch4LengthCounter 0
         else do
           -- DMG: clear FF10-FF1F, skip NR41 (0xFF20), clear FF21-FF25
           mapM_ (\a -> writeArr memory a 0) [0xFF10..0xFF1F]
           mapM_ (\a -> writeArr memory a 0) [0xFF21..0xFF25]
       writeRef ch1Active False
       writeRef ch2Active False
       writeRef ch3Active False
       writeRef ch4Active False
       writeRef ch1LengthCounter 0
       writeRef ch2LengthCounter 0
       writeRef ch3LengthCounter 0
       writeRef apuFrameSeqStep 0
       writeRef apuFrameSeqCounter 0
     else return ()
 writeMemMapped addr val
   -- NR41 (0xFF20): writable even when APU is off on DMG, gated on CGB
   | addr == 0xFF20 = do
       gbc <- readRef gbc_mode
       if gbc
         then do
           power <- readRef apuPower
           when power $ do
             writeArr memory addr val
             writeRef ch4LengthCounter (64 - fromIntegral (val .&. 0x3F))
         else do
           writeArr memory addr val
           writeRef ch4LengthCounter (64 - fromIntegral (val .&. 0x3F))
   -- Sound register writes (0xFF10-0xFF25): gated by APU power
   | addr >= 0xFF10 && addr <= 0xFF25 = do
       power <- readRef apuPower
       when power $ writeSoundReg addr val
   -- MBC1: ROM bank select (0x2000-0x3FFF)
   | addr >= 0x2000 && addr <= 0x3FFF = do
       let bank = val .&. 0x1F
       let effective = if bank == 0 then 1 else bank
       nBanks <- readRef numRomBanks
       let clamped = if nBanks > 1
                       then ((effective - 1) `mod` (nBanks - 1)) + 1
                       else 1
       writeRef romBank clamped
   -- Ignore writes to ROM area
   | addr < 0x2000                    = return ()
   | addr >= 0x4000 && addr <= 0x7FFF = return ()
   -- VRAM: blocked during Transfer (Mode 3)
   | addr >= 0x8000 && addr <= 0x9FFF = do
       m <- readRef mode
       case m of
         Transfer -> return ()
         _        -> writeArr memory addr val
   -- Echo RAM
   | addr >= 0xE000 && addr <= 0xFDFF = writeArr memory (addr - 0x2000) val
   -- OAM: blocked during OAMSearch and Transfer
   | addr >= 0xFE00 && addr <= 0xFE9F = do
       m <- readRef mode
       case m of
         OAMSearch -> return ()
         Transfer  -> return ()
         _         -> writeArr memory addr val
   -- Unusable region
   | addr >= 0xFEA0 && addr <= 0xFEFF = return ()
   | otherwise                        = writeArr memory addr val

 writeSoundReg :: Word16 -> Word8 -> GBC ()
 writeSoundReg addr val = do
   -- Read old value before overwriting (needed for length-enable edge detection)
   oldVal <- readArr memory addr
   -- Store the value in memory
   writeArr memory addr val
   -- Then handle side effects
   case addr of
     -- NR11: length counter = 64 - (val & 0x3F)
     0xFF11 -> writeRef ch1LengthCounter (64 - fromIntegral (val .&. 0x3F))
     -- NR12: DAC check
     0xFF12 -> when (val .&. 0xF8 == 0) $ writeRef ch1Active False
     -- NR14: trigger + extra length clocking
     0xFF14 -> do
       -- Extra length clock: if length-enable goes 0→1 and not triggering
       when (testBit val 6 && not (testBit oldVal 6) && not (testBit val 7)) $
         extraLengthClockNonTrigger ch1LengthCounter ch1Active
       when (testBit val 7) APU.triggerChannel1
     -- NR21: length counter = 64 - (val & 0x3F)
     0xFF16 -> writeRef ch2LengthCounter (64 - fromIntegral (val .&. 0x3F))
     -- NR22: DAC check
     0xFF17 -> when (val .&. 0xF8 == 0) $ writeRef ch2Active False
     -- NR24: trigger + extra length clocking
     0xFF19 -> do
       when (testBit val 6 && not (testBit oldVal 6) && not (testBit val 7)) $
         extraLengthClockNonTrigger ch2LengthCounter ch2Active
       when (testBit val 7) APU.triggerChannel2
     -- NR30: DAC check (bit 7)
     0xFF1A -> when (not $ testBit val 7) $ writeRef ch3Active False
     -- NR31: length counter = 256 - val
     0xFF1B -> writeRef ch3LengthCounter (256 - fromIntegral val)
     -- NR34: trigger + extra length clocking
     0xFF1E -> do
       when (testBit val 6 && not (testBit oldVal 6) && not (testBit val 7)) $
         extraLengthClockNonTrigger ch3LengthCounter ch3Active
       when (testBit val 7) APU.triggerChannel3
     -- NR41: length counter = 64 - (val & 0x3F)
     0xFF20 -> writeRef ch4LengthCounter (64 - fromIntegral (val .&. 0x3F))
     -- NR42: DAC check
     0xFF21 -> when (val .&. 0xF8 == 0) $ writeRef ch4Active False
     -- NR44: trigger + extra length clocking
     0xFF23 -> do
       when (testBit val 6 && not (testBit oldVal 6) && not (testBit val 7)) $
         extraLengthClockNonTrigger ch4LengthCounter ch4Active
       when (testBit val 7) APU.triggerChannel4
     _      -> return ()

 -- Extra length clock when length-enable transitions 0→1 without trigger
 extraLengthClockNonTrigger :: (Memory RealWorld -> STRef RealWorld Word16)
                            -> (Memory RealWorld -> STRef RealWorld Bool) -> GBC ()
 extraLengthClockNonTrigger lenRef activeRef = do
   step <- readRef apuFrameSeqStep
   when (odd step) $ do
     len <- readRef lenRef
     when (len > 0) $ do
       let len' = len - 1
       writeRef lenRef len'
       when (len' == 0) $ writeRef activeRef False
