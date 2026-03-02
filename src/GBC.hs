{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module GBC
 ( run, runRomHeadless )
 where

 import Control.Monad
 import Control.Monad.ST
 import Data.IORef

 import Data.Word

 import Foreign (alloca, poke, nullFunPtr, nullPtr)
 import qualified SDL.Raw.Audio as RawAudio
 import qualified SDL.Raw.Types as RawTypes

 import Effectful (Eff, (:>), runEff)
 import Effectful.Dispatch.Static (evalStaticRep)

 import Types
 import Memory
 import Cartridge
 import CPU (handleInterrupts)
 import GPU (screen_width, screen_height)
 import Effects.Env (GBC, EmulatorEnv, StaticRep(EmulatorEnv))
 import Effects.MemoryBus (MemoryBus, tickMCycle)
 import Effects.CpuBackend (CpuBackend, cpuStep)
 import Effects.Trace (runTraceSilent)
 import Effects.Display (presentFrame)
 import Effects.Input (pollInput)
 import Effects.Handlers.SDL (runDisplaySDL, runAudioOutSDL, runInputSDL, SDLDisplay(..))
 import Effects.Handlers.Headless (runDisplayHeadless, runAudioOutHeadless, runInputHeadless)
 import Effects.Handlers.CpuBackend (runInterpreter, runTracingInterpreter)
 import MemoryRules (runMemoryBusFull)

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

 runGBC :: Word8 -> Maybe FilePath -> GBC a -> IO a
 runGBC numBanks ssDir action = do
   initializeAll
   window' <- createWindow "HaskellBoy" defaultWindowConfig
   renderer' <- createRenderer window' (-1) defaultRenderer
   -- Open SDL audio device in queue mode (null callback)
   devID <- alloca $ \desiredPtr -> alloca $ \obtainedPtr -> do
     poke desiredPtr $ RawTypes.AudioSpec
       { RawTypes.audioSpecFreq = 44100
       , RawTypes.audioSpecFormat = 0x8010  -- AUDIO_S16LSB
       , RawTypes.audioSpecChannels = 2
       , RawTypes.audioSpecSilence = 0
       , RawTypes.audioSpecSamples = 1024
       , RawTypes.audioSpecSize = 0
       , RawTypes.audioSpecCallback = nullFunPtr
       , RawTypes.audioSpecUserdata = nullPtr
       }
     dev <- RawAudio.openAudioDevice nullPtr 0 desiredPtr obtainedPtr 0
     RawAudio.pauseAudioDevice dev 0  -- unpause
     return dev
   lastFrameRef <- newIORef =<< SDL.ticks
   ssDirRef <- newIORef ssDir
   mem <- stToIO $ Memory.new False numBanks
   let sdlSt = SDLDisplay
         { sdlWindow = window'
         , sdlRenderer = renderer'
         , sdlLastFrameRef = lastFrameRef
         , sdlScreenshotDir = ssDirRef
         }
   runEff
     . evalStaticRep (EmulatorEnv mem)
     . runAudioOutSDL devID
     . runDisplaySDL sdlSt
     . runInputSDL ssDirRef
     . runTraceSilent
     . runMemoryBusFull
     $ action

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

 -- Backend-agnostic emulator loop (infinite)
 emulatorLoop :: (CpuBackend :> es, MemoryBus :> es, EmulatorEnv :> es) => Eff es ()
 emulatorLoop = forever $ do
   pending <- readRef imePending
   when pending $ writeRef ime True >> writeRef imePending False
   halted <- readRef halt
   if halted
     then tickMCycle >> handleInterrupts
     else cpuStep >> handleInterrupts

 -- Backend-agnostic emulator loop (bounded by cycle count)
 emulatorLoopBounded :: (CpuBackend :> es, MemoryBus :> es, EmulatorEnv :> es)
                     => Word64 -> Eff es ()
 emulatorLoopBounded maxCycles = go
   where
     go = do
       pending <- readRef imePending
       when pending $ writeRef ime True >> writeRef imePending False
       halted <- readRef halt
       if halted
         then tickMCycle >> handleInterrupts
         else cpuStep >> handleInterrupts
       n <- readRef mCycleCounter
       when (n < maxCycles) go

 run :: FilePath -> Bool -> Bool -> Maybe FilePath -> IO ()
 run romPath skipBoot trace ssDir = do
   cart <- readCartridge romPath
   let numBanks = romBankCount cart
   runGBC numBanks ssDir $ do
     storeCartridge cart
     if skipBoot
       then postBootState
       else loadBootRom Memory.boot_rom
     presentFrame
     pollInput
     let runner = if trace then runTracingInterpreter else runInterpreter
     runner emulatorLoop

 runGBCHeadless :: Word8 -> GBC a -> IO a
 runGBCHeadless numBanks action = do
   mem <- Memory.newHeadless numBanks
   runEff
     . evalStaticRep (EmulatorEnv mem)
     . runAudioOutHeadless
     . runDisplayHeadless
     . runInputHeadless
     . runTraceSilent
     . runMemoryBusFull
     $ action

 runRomHeadless :: FilePath -> Word64 -> IO String
 runRomHeadless romPath maxCycles = do
   cart <- readCartridge romPath
   let numBanks = romBankCount cart
   runGBCHeadless numBanks $ do
     storeCartridge cart
     postBootState
     runInterpreter $ emulatorLoopBounded maxCycles
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
