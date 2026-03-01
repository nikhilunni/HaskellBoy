{-# LANGUAGE OverloadedStrings #-}

module GBC
 ( run, runRomHeadless, diagnoseTitleScreen )
 where

 import Control.Monad
 import Control.Monad.Reader
 import Control.Monad.ST
 import Data.STRef (writeSTRef)

 import Data.Word
 import Data.Bits (testBit, (.&.), shiftR, (.|.), shiftL)
 import Numeric (showHex)

 import System.IO (hPutStrLn, stderr)

 import Foreign (alloca, poke, nullFunPtr, nullPtr)
 import qualified SDL.Raw.Audio as RawAudio
 import qualified SDL.Raw.Types as RawTypes

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
   mem <- stToIO $ Memory.new window' renderer' False numBanks
   stToIO $ writeSTRef (audioDeviceID mem) devID
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

 -- | Diagnostic: run Pokemon Red headlessly, then dump GPU/VRAM state
 -- to help debug rendering issues.
 diagnoseTitleScreen :: FilePath -> IO String
 diagnoseTitleScreen romPath = do
  cart <- readCartridge romPath
  let numBanks = romBankCount cart
  runGBCHeadless numBanks $ do
    storeCartridge cart
    postBootState
    -- Run for ~10 seconds of Game Boy time (enough to reach title screen)
    -- GB runs at 4194304 Hz, so 10s ≈ 42M cycles
    let maxCycles = 42000000 :: Word64
    let loop = do
          pending <- readRef imePending
          when pending $ do
            writeRef ime True
            writeRef imePending False
          halted <- readRef halt
          if halted
            then do
              tickMCycle
              handleInterrupts
            else do
              (next, _cycles) <- streamNextInstruction
              executeInstruction next
              handleInterrupts
          n <- readRef mCycleCounter
          when (n < maxCycles) loop
    loop
    -- Now dump GPU state
    lcdc <- readArr memory 0xFF40
    stat <- readArr memory 0xFF41
    scy  <- readArr memory 0xFF42
    scx  <- readArr memory 0xFF43
    ly   <- readArr memory 0xFF44
    lyc  <- readArr memory 0xFF45
    bgp  <- readArr memory 0xFF47
    obp0 <- readArr memory 0xFF48
    obp1 <- readArr memory 0xFF49
    wy   <- readArr memory 0xFF4A
    wx   <- readArr memory 0xFF4B

    let h8 v = let s = showHex v "" in replicate (2 - length s) '0' ++ s

    -- Dump key registers
    let regDump = unlines
          [ "=== GPU Register State ==="
          , "LCDC (FF40): 0x" ++ h8 lcdc
          , "  Bit 7 (LCD on):      " ++ show (testBit lcdc 7)
          , "  Bit 6 (Win map):     " ++ (if testBit lcdc 6 then "9C00" else "9800")
          , "  Bit 5 (Win enable):  " ++ show (testBit lcdc 5)
          , "  Bit 4 (Tile data):   " ++ (if testBit lcdc 4 then "8000" else "8800")
          , "  Bit 3 (BG map):      " ++ (if testBit lcdc 3 then "9C00" else "9800")
          , "  Bit 2 (Sprite size): " ++ (if testBit lcdc 2 then "8x16" else "8x8")
          , "  Bit 1 (Sprite on):   " ++ show (testBit lcdc 1)
          , "  Bit 0 (BG on):       " ++ show (testBit lcdc 0)
          , "STAT (FF41): 0x" ++ h8 stat
          , "SCY  (FF42): " ++ show scy
          , "SCX  (FF43): " ++ show scx
          , "LY   (FF44): " ++ show ly
          , "LYC  (FF45): " ++ show lyc
          , "BGP  (FF47): 0x" ++ h8 bgp
          , "OBP0 (FF48): 0x" ++ h8 obp0
          , "OBP1 (FF49): 0x" ++ h8 obp1
          , "WY   (FF4A): " ++ show wy
          , "WX   (FF4B): " ++ show wx
          ]

    -- Determine tile addressing mode
    let (tileStart, offset) = if testBit lcdc 4
          then (0x8000 :: Word16, 0 :: Word8)
          else (0x8800 :: Word16, 128 :: Word8)
    let bgMapStart = if testBit lcdc 3 then 0x9C00 :: Word16 else 0x9800 :: Word16
    let winMapStart = if testBit lcdc 6 then 0x9C00 :: Word16 else 0x9800 :: Word16

    -- Dump BG tile map (first 18 rows × 20 cols = visible area)
    bgMapDump <- fmap unlines $ forM [0..17 :: Int] $ \row -> do
      tiles <- forM [0..19 :: Int] $ \col -> do
        let addr = bgMapStart + fromIntegral (row * 32 + col)
        t <- readArr memory addr
        return $ h8 t
      return $ "  Row " ++ show row ++ ": " ++ unwords tiles

    -- Dump window tile map (first 6 rows × 20 cols)
    winMapDump <- fmap unlines $ forM [0..5 :: Int] $ \row -> do
      tiles <- forM [0..19 :: Int] $ \col -> do
        let addr = winMapStart + fromIntegral (row * 32 + col)
        t <- readArr memory addr
        return $ h8 t
      return $ "  Row " ++ show row ++ ": " ++ unwords tiles

    -- Check if tile data is populated (sample a few tiles)
    tileSamples <- fmap unlines $ forM [0, 1, 2, 128, 129, 130] $ \tileIdx -> do
      let addr = tileStart + fromIntegral (((tileIdx + fromIntegral offset) :: Word8)) * 16
      bytes <- mapM (\off -> readArr memory (fromIntegral addr + off)) [0..15 :: Word16]
      let byteStrs = map h8 bytes
      return $ "  Tile " ++ show tileIdx ++ " (addr 0x" ++ showHex addr "" ++ "): " ++ unwords byteStrs

    -- Reconstruct what scanline 72 (middle of screen) should look like
    -- This manually replicates the renderBackground logic
    let testLine = 72 :: Word8
    let totalY = scy + testLine
    let yTileRow = fromIntegral ((totalY `div` 8) .&. 31) :: Word16
    let rowBase = bgMapStart + yTileRow * 32
    let xTileStart = fromIntegral (scx `div` 8) :: Word16
    let scxFine = fromIntegral (scx .&. 0x07) :: Int

    scanlineDump <- fmap unlines $ forM [0..20 :: Word16] $ \tileOff -> do
      let xTile = (xTileStart + tileOff) .&. 31
      let tileMapAddr = rowBase + xTile
      rawTileNum <- readArr memory tileMapAddr
      let tileNum = rawTileNum + offset
      let yByteOff = 2 * (totalY `mod` 8)
      let tileDataAddr = fromIntegral tileNum * 16 + tileStart + fromIntegral yByteOff
      lo <- readArr memory tileDataAddr
      hi <- readArr memory (tileDataAddr + 1)
      let startX = fromIntegral tileOff * 8 - scxFine :: Int
      -- Decode pixels
      let pixels = [ fromIntegral (((hi `shiftR` (7-i)) .&. 1) `shiftL` 1
                    .|. ((lo `shiftR` (7-i)) .&. 1)) :: Int
                   | i <- [0..7] ]
      return $ "  Tile " ++ show tileOff
            ++ " (map=0x" ++ showHex tileMapAddr ""
            ++ " raw=" ++ h8 rawTileNum
            ++ " num=" ++ h8 tileNum
            ++ " data=0x" ++ showHex tileDataAddr ""
            ++ " lo=" ++ h8 lo ++ " hi=" ++ h8 hi
            ++ " x=" ++ show startX
            ++ "): pixels=" ++ show pixels

    -- Check OAM for sprites
    oamDump <- fmap unlines $ forM [0..39 :: Word16] $ \i -> do
      let addr = 0xFE00 + i * 4
      yPos  <- readArr memory addr
      xPos  <- readArr memory (addr + 1)
      tileN <- readArr memory (addr + 2)
      attrs <- readArr memory (addr + 3)
      if yPos > 0 && yPos < 160 && xPos > 0 && xPos < 168
        then return $ "  OAM " ++ show i ++ ": y=" ++ show yPos
                    ++ " x=" ++ show xPos ++ " tile=" ++ h8 tileN
                    ++ " attr=" ++ h8 attrs
        else return ""

    return $ regDump
          ++ "\n=== BG Tile Map (visible area) ===\n" ++ bgMapDump
          ++ "\n=== Window Tile Map (first 6 rows) ===\n" ++ winMapDump
          ++ "\n=== Tile Data Samples ===\n" ++ tileSamples
          ++ "\n=== Scanline 72 Reconstruction ===\n"
          ++ "  totalY=" ++ show totalY ++ " yTileRow=" ++ show yTileRow
          ++ " xTileStart=" ++ show xTileStart ++ " scxFine=" ++ show scxFine ++ "\n"
          ++ scanlineDump
          ++ "\n=== Active OAM Sprites ===\n" ++ concat (filter (not . null) (lines (oamDump)))
