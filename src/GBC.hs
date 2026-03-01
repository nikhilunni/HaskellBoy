{-# LANGUAGE OverloadedStrings #-}

module GBC
 where

 import Control.Monad
 import Control.Monad.Reader
 import Control.Monad.ST

 import Data.Word
 import Numeric (showHex)

 import System.IO (hPutStrLn, stderr)

 import Types
 import Memory
 import Monad
 import Cartridge
 import MemoryRules (tickMCycle)
 import CPU
 import GPU

 import SDL hiding (E)


 defaultWindowConfig :: WindowConfig
 defaultWindowConfig = defaultWindow
   { windowBorder       = True
   , windowHighDPI      = False
   , windowInputGrabbed = False
   , windowMode         = Windowed
   , windowPosition     = Wherever
   , windowResizable    = False
   , windowInitialSize  = V2 (fromIntegral screen_width)
                             (fromIntegral screen_height)
   }

 runGBC :: Word8 -> GBC a -> IO a
 runGBC numBanks (GBC reader) = do
   initializeAll
   window' <- createWindow "HaskellBoy" defaultWindowConfig
   renderer' <- createRenderer window' (-1) defaultRenderer
   mem <- stToIO $ Memory.new window' renderer' True numBanks
   runReaderT reader mem

 postBootState :: GBC ()
 postBootState = do
   -- gbc_mode is set by storeCartridge based on cartridge header byte 0x143
   gbc <- readRef gbc_mode
   writeReg A (if gbc then 0x11 else 0x01)
   writeReg F 0xB0
   writeReg B 0x00
   writeReg C 0x13
   writeReg D 0x00
   writeReg E 0xD8
   writeReg H 0x01
   writeReg L 0x4D
   writeRef sp 0xFFFE
   writeRef pc 0x0100
   -- I/O registers (DMG post-boot values)
   -- Use writeArr to avoid triggering M-cycle ticks during init
   writeArr memory 0xFF40 0x91 -- LCDC: LCD on, tile data 0x8000, BG on
   writeArr memory 0xFF41 0x85 -- STAT
   writeArr memory 0xFF47 0xFC -- BGP: palette
   writeArr memory 0xFF48 0xFF -- OBP0
   writeArr memory 0xFF49 0xFF -- OBP1
   writeArr memory 0xFF0F 0xE1 -- IF
   writeArr memory 0xFFFF 0x00 -- IE
   writeArr memory 0xFF50 0x01
   -- Sound registers (DMG post-boot values)
   writeRef apuPower True
   writeArr memory 0xFF10 0x80 -- NR10
   writeArr memory 0xFF11 0xBF -- NR11
   writeArr memory 0xFF12 0xF3 -- NR12
   writeArr memory 0xFF14 0xBF -- NR14
   writeArr memory 0xFF16 0x3F -- NR21
   writeArr memory 0xFF17 0x00 -- NR22
   writeArr memory 0xFF19 0xBF -- NR24
   writeArr memory 0xFF1A 0x7F -- NR30
   writeArr memory 0xFF1B 0xFF -- NR31
   writeArr memory 0xFF1C 0x9F -- NR32
   writeArr memory 0xFF1E 0xBF -- NR34
   writeArr memory 0xFF20 0xFF -- NR41
   writeArr memory 0xFF21 0x00 -- NR42
   writeArr memory 0xFF22 0x00 -- NR43
   writeArr memory 0xFF23 0xBF -- NR44
   writeArr memory 0xFF24 0x77 -- NR50
   writeArr memory 0xFF25 0xF3 -- NR51
   writeArr memory 0xFF26 0xF1 -- NR52
   writeRef ch1Active True

 showHex16 :: Word16 -> String
 showHex16 w = let s = showHex w ""
               in replicate (4 - length s) '0' ++ s

 run :: FilePath -> Bool -> Bool -> Maybe FilePath -> IO ()
 run romPath skipBoot trace ssDir = do
   cart <- readCartridge romPath
   let numBanks = romBankCount cart
   runGBC numBanks $ do
     storeCartridge cart
     when trace $ writeRef traceEnabled True
     case ssDir of
       Just d  -> writeRef screenshotDir (Just d)
       Nothing -> return ()
     if skipBoot
       then postBootState
       else loadBootRom Memory.boot_rom
     showScreen
     pollSDLEvents
     forever $ do
       -- Apply pending IME from previous EI instruction
       pending <- readRef imePending
       when pending $ do
         writeRef ime True
         writeRef imePending False
       halted <- readRef halt
       if halted
         then do
           -- When halted, tick one M-cycle and check interrupts
           tickMCycle
           handleInterrupts
         else do
           tracing <- readRef traceEnabled
           pcVal <- readRef pc
           (next, _cycles) <- streamNextInstruction
           when tracing $
             liftIO $ hPutStrLn stderr $ "PC=" ++ showHex16 pcVal ++ " " ++ show next
           executeInstruction next
           -- Cycles are now charged per-M-cycle inside readMem/writeMem/tickMCycle
           handleInterrupts

 runGBCHeadless :: Word8 -> GBC a -> IO a
 runGBCHeadless numBanks (GBC reader) = do
   mem <- Memory.newHeadless numBanks
   runReaderT reader mem

 runRomHeadless :: FilePath -> Word64 -> IO String
 runRomHeadless romPath maxCycles = do
   cart <- readCartridge romPath
   let numBanks = romBankCount cart
   runGBCHeadless numBanks $ do
     storeCartridge cart
     postBootState
     let loop = do
           pending <- readRef imePending
           when pending $ do
             writeRef ime True
             writeRef imePending False
           halted <- readRef halt
           if halted
             then do
               -- When halted, tick one M-cycle and check interrupts
               tickMCycle
               handleInterrupts
             else do
               (next, _cycles) <- streamNextInstruction
               executeInstruction next
               handleInterrupts
           n <- readRef mCycleCounter
           when (n < maxCycles) loop
     loop
     serial <- readRef serialBuffer
     if not (null serial)
       then return serial
       else do
         -- Check Blargg memory-mapped output at 0xA000+
         sig1 <- readArr memory 0xA001
         sig2 <- readArr memory 0xA002
         sig3 <- readArr memory 0xA003
         if sig1 == 0xDE && sig2 == 0xB0 && sig3 == 0x61
           then do
             textBytes <- mapM (readArr memory) [0xA004..0xA0FF]
             let text = map (toEnum . fromIntegral) $ takeWhile (/= 0) textBytes
             return text
           else return serial
