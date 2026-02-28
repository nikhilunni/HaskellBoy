{-# LANGUAGE OverloadedStrings #-}

module GBC
 where

 import Control.Monad
 import Control.Monad.Reader
 import Control.Monad.ST

 import Data.Word
 import Data.IORef
 import Numeric (showHex)

 import System.IO (hPutStrLn, stderr)

 import Types
 import Memory
 import Monad
 import Cartridge
 import MemoryRules (writeMem)
 import CPU
 import GPU

 import SDL hiding (E)

 tick :: Cycles -> GBC ()
 tick n = GPU.tick n


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
   writeReg A 0x01
   writeReg F 0xB0
   writeReg B 0x00
   writeReg C 0x13
   writeReg D 0x00
   writeReg E 0xD8
   writeReg H 0x01
   writeReg L 0x4D
   writeRef sp 0xFFFE
   writeRef pc 0x0100
   writeRef gbc_mode False
   -- I/O registers (DMG post-boot values)
   writeMem 0xFF40 0x91 -- LCDC: LCD on, tile data 0x8000, BG on
   writeMem 0xFF41 0x85 -- STAT
   writeMem 0xFF47 0xFC -- BGP: palette
   writeMem 0xFF48 0xFF -- OBP0
   writeMem 0xFF49 0xFF -- OBP1
   writeMem 0xFF0F 0xE1 -- IF
   writeMem 0xFFFF 0x00 -- IE
   writeMem 0xFF50 0x01

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
           GBC.tick 4
           handleInterrupts
         else do
           tracing <- readRef traceEnabled
           pcVal <- readRef pc
           (next, cycles) <- streamNextInstruction
           when tracing $
             liftIO $ hPutStrLn stderr $ "PC=" ++ showHex16 pcVal ++ " " ++ show next
           executeInstruction next
           GBC.tick cycles
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
     cycleRef <- liftIO $ newIORef (0 :: Word64)
     let loop = do
           pending <- readRef imePending
           when pending $ do
             writeRef ime True
             writeRef imePending False
           halted <- readRef halt
           if halted
             then do
               -- When halted, just tick 4 cycles and check interrupts
               GBC.tick 4
               handleInterrupts
               liftIO $ modifyIORef' cycleRef (+4)
             else do
               (next, cycles') <- streamNextInstruction
               executeInstruction next
               GBC.tick cycles'
               handleInterrupts
               liftIO $ modifyIORef' cycleRef (+ fromIntegral cycles')
           n <- liftIO $ readIORef cycleRef
           when (n < maxCycles) loop
     loop
     readRef serialBuffer
