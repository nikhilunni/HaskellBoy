{-# LANGUAGE BangPatterns #-}

module GPU
 where

 import Control.Monad
 import Data.Word
 import Data.Bits

 import Types
 import Monad
 import Memory


 {-# INLINE tickTimer #-}
 tickTimer :: Cycles -> GBC ()
 tickTimer delta = do
   -- DIV register: increments every 256 T-cycles
   divCnt <- readRef divCounter
   let newDiv = divCnt + delta
   when (newDiv >= 256) $ do
     div' <- readArr memory 0xFF04
     writeArr memory 0xFF04 (div' + 1)
   writeRef divCounter (newDiv `mod` 256)
   -- TIMA timer
   tac <- readArr memory 0xFF07
   when (testBit tac 2) $ do
     let period = case tac .&. 0x03 of
                    0 -> 1024
                    1 -> 16
                    2 -> 64
                    _ -> 256
     cnt <- readRef timerCounter
     let newCnt = cnt + delta
     if newCnt >= period
       then do
         writeRef timerCounter (newCnt - period)
         tima <- readArr memory 0xFF05
         if tima == 0xFF
           then do
             tma <- readArr memory 0xFF06
             writeArr memory 0xFF05 tma
             requestInterrupt Timer
           else writeArr memory 0xFF05 (tima + 1)
       else writeRef timerCounter newCnt

 screen_width :: Int
 screen_width  = 160

 screen_height :: Int
 screen_height = 144

 {-# INLINE compareLY_LYC #-}
 compareLY_LYC :: GBC ()
 compareLY_LYC = do
   ly <- readArr memory ly_addr
   lyc <- readArr memory lyc_addr
   stat <- readArr memory stat_addr
   writeArr memory stat_addr $ (if ly == lyc then setBit else clearBit) stat 2
   when (ly == lyc && testBit stat 6) $ requestInterrupt STAT


 {-# INLINE lcdModeSwitch #-}
 lcdModeSwitch :: GPUMode -> GBC ()
 lcdModeSwitch m = do
   writeRef mode m
   stat <- readArr memory stat_addr
   let newStat = (stat .&. 0xFC) .|. (fromIntegral . fromEnum $ m)
   writeArr memory stat_addr newStat
   -- Only modes 0, 1, 2 have STAT interrupt sources (bits 3, 4, 5)
   -- Mode 3 (Transfer) has no STAT interrupt source
   when (fromEnum m < 3 && testBit stat (fromEnum m + 3)) $
     requestInterrupt STAT

 --HDMA is enabled if the 7th bit is set (hdma5 `shiftL` 7 == 1)
 hdmaEnabled :: GBC Bool
 hdmaEnabled = (>= 128) <$> (readArr memory hdma5_addr)

 --Imperative af :-(
 tick :: Cycles -> GBC ()
 tick delta = do
   tickTimer delta
   lineNum <- readRef line
   scanMode <- readRef mode
   counter <- readRef gpu_cycles
   let total = counter + delta
   writeRef gpu_cycles total
   case scanMode of
     HBlankMode -> do
       when (total >= 456) $ do --End of HBlank period
         writeRef gpu_cycles $ total - 456
         writeRef line $ lineNum + 1
         writeArr memory ly_addr $ lineNum + 1
         compareLY_LYC

         --TODO : HDMA transfer
         if (lineNum + 1 == 144)
           then do requestInterrupt VBlank
                   lcdModeSwitch VBlankMode
                   showScreen
                   pollSDLEvents
                   queueAudioSamples
                   limitFrameRate
                   dir <- readRef screenshotDir
                   case dir of
                     Just d  -> captureScreenshot (d ++ "/screenshot.bmp")
                     Nothing -> return ()
           else do lcdModeSwitch OAMSearch

     VBlankMode -> do
       if (lineNum + 1 < 153)
          then do when (total >= 456) $ do --End of line
                    writeRef gpu_cycles $ total - 456
                    writeRef line $ lineNum + 1
                    writeArr memory ly_addr $ lineNum + 1
                    compareLY_LYC
          else do ly <- readArr memory ly_addr
                  when (total >= 8 && ly == 153) $ do
                    writeArr memory ly_addr 0 --LY value is reset early, for whatever reason...
                    compareLY_LYC
                  when (total >= 456) $ do --Last line of VBlank is over
                    writeRef gpu_cycles $ total - 456
                    writeRef line 0
                    lcdModeSwitch OAMSearch

     OAMSearch -> when (total >= 80) $ (writeRef transferred False) >> lcdModeSwitch Transfer
     Transfer -> do
       scanned <- readRef transferred
       ly <- readArr memory ly_addr
       when (not scanned && total >= (if ly == 0 then 160 else 48)) $ do
         writeRef transferred True
         scanLine lineNum
       when (total >= 172) $ lcdModeSwitch HBlankMode



 {-# INLINE scanLine #-}
 scanLine :: Word8 -> GBC ()
 scanLine lineNum = do
   lcdc <- readArr memory lcdc_addr
   when (testBit lcdc 7) $ do
     hl <- readRef headless
     renderLine hl lineNum

 {-# INLINE renderLine #-}
 renderLine :: Bool -> Word8 -> GBC ()
 renderLine !hl !line = do
   renderBackground hl line
   renderWindow hl line
   renderSprites hl line


 renderBackground :: Bool -> Word8 -> GBC ()
 renderBackground !hl !line = do
   lcdc <- readArr memory lcdc_addr
   gbc <- readRef gbc_mode
   if (testBit lcdc 0) --If BG display is enabled
      then do let (tileStart, offset) = if testBit lcdc 4 then (0x8000::Word16, 0) else (0x8800::Word16, 128)
              let bgStart = if testBit lcdc 3 then (0x9C00::Word16) else (0x9800::Word16)
              scy <- readArr memory scy_addr
              scx <- readArr memory scx_addr

              let totalY = scy + line

              --Each line is 32 tiles, and we round down to the nearest tile to find the tile offset
              let yTileOffset = (totalY `div` 8) * 32
              let xTileOffset = scx `div` 8

              --Range is equal to the max number of tiles that can be shown on the screen at the same time
              let tileAddrOffsetLower = bgStart + (fromIntegral yTileOffset) +
                                        (fromIntegral xTileOffset) - 1
              let tileAddrOffsetUpper = bgStart + (fromIntegral yTileOffset) +
                                        (fromIntegral xTileOffset) + 20

              forM_ [tileAddrOffsetLower .. tileAddrOffsetUpper] $ \tileIdx -> do
                tileAttr <- if gbc then (readArr vramBank tileIdx) else (return 0)
                tileNum <- (+ offset) <$> (readArr memory tileIdx)

                let !yByteOffset = 2 * (if testBit tileAttr 6 then (7 - (totalY `mod` 8)) else (totalY `mod` 8))
                let !finalTileAddr = (fromIntegral tileNum * 16) + tileStart + (fromIntegral yByteOffset)
                let tile_bank = (if testBit tileAttr 3 then vramBank else memory)
                lowerByte <- readArr tile_bank finalTileAddr
                upperByte <- readArr tile_bank $ finalTileAddr + 1
                let !packed = if testBit tileAttr 5
                                then packTileRowFlipped lowerByte upperByte
                                else packTileRow lowerByte upperByte
                let !tileOff = fromIntegral tileIdx - fromIntegral tileAddrOffsetLower :: Int
                    !scxI = fromIntegral (scx .&. 0x07) :: Int
                    !startX = (tileOff - 1) * 8 - scxI
                pal <- (if gbc then (getGBCPalette $ tileAttr .&. 0x7) else (getGBPalette <$> readArr memory bgp_addr))
                renderTileRow hl pal packed startX (fromIntegral line)
     else return ()

 -- Pack lo/hi tile bytes into a Word16: 2 bits per pixel, pixel 0 in bits 15:14
 {-# INLINE packTileRow #-}
 packTileRow :: Word8 -> Word8 -> Word16
 packTileRow !lo !hi =
   let !b7 = fromIntegral (((hi `shiftR` 7) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 7) .&. 1)) :: Word16
       !b6 = fromIntegral (((hi `shiftR` 6) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 6) .&. 1)) :: Word16
       !b5 = fromIntegral (((hi `shiftR` 5) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 5) .&. 1)) :: Word16
       !b4 = fromIntegral (((hi `shiftR` 4) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 4) .&. 1)) :: Word16
       !b3 = fromIntegral (((hi `shiftR` 3) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 3) .&. 1)) :: Word16
       !b2 = fromIntegral (((hi `shiftR` 2) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 2) .&. 1)) :: Word16
       !b1 = fromIntegral (((hi `shiftR` 1) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 1) .&. 1)) :: Word16
       !b0 = fromIntegral (((hi             ) .&. 1) `shiftL` 1 .|. ((lo             ) .&. 1)) :: Word16
   in (b7 `shiftL` 14) .|. (b6 `shiftL` 12) .|. (b5 `shiftL` 10) .|. (b4 `shiftL` 8)
      .|. (b3 `shiftL` 6) .|. (b2 `shiftL` 4) .|. (b1 `shiftL` 2) .|. b0

 {-# INLINE packTileRowFlipped #-}
 packTileRowFlipped :: Word8 -> Word8 -> Word16
 packTileRowFlipped !lo !hi =
   let !b0 = fromIntegral (((hi             ) .&. 1) `shiftL` 1 .|. ((lo             ) .&. 1)) :: Word16
       !b1 = fromIntegral (((hi `shiftR` 1) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 1) .&. 1)) :: Word16
       !b2 = fromIntegral (((hi `shiftR` 2) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 2) .&. 1)) :: Word16
       !b3 = fromIntegral (((hi `shiftR` 3) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 3) .&. 1)) :: Word16
       !b4 = fromIntegral (((hi `shiftR` 4) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 4) .&. 1)) :: Word16
       !b5 = fromIntegral (((hi `shiftR` 5) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 5) .&. 1)) :: Word16
       !b6 = fromIntegral (((hi `shiftR` 6) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 6) .&. 1)) :: Word16
       !b7 = fromIntegral (((hi `shiftR` 7) .&. 1) `shiftL` 1 .|. ((lo `shiftR` 7) .&. 1)) :: Word16
   in (b0 `shiftL` 14) .|. (b1 `shiftL` 12) .|. (b2 `shiftL` 10) .|. (b3 `shiftL` 8)
      .|. (b4 `shiftL` 6) .|. (b5 `shiftL` 4) .|. (b6 `shiftL` 2) .|. b7

 {-# INLINE extractPixel #-}
 extractPixel :: Word16 -> Int -> Word8
 extractPixel !packed !i = fromIntegral $ (packed `shiftR` (14 - i * 2)) .&. 3

 {-# INLINE renderPixelDirect #-}
 renderPixelDirect :: Bool -> Palette -> Int -> Int -> Word8 -> GBC ()
 renderPixelDirect !hl !(c0,c1,c2,c3) !px !py !col = do
   let !(Color a r g b) = case col of
         0 -> c0
         1 -> c1
         2 -> c2
         _ -> c3
   writePixelDirect hl a r g b px py

 {-# INLINE renderTileRow #-}
 renderTileRow :: Bool -> Palette -> Word16 -> Int -> Int -> GBC ()
 renderTileRow !hl !pal !packed !baseX !y = do
   renderPixelDirect hl pal (baseX    ) y (extractPixel packed 0)
   renderPixelDirect hl pal (baseX + 1) y (extractPixel packed 1)
   renderPixelDirect hl pal (baseX + 2) y (extractPixel packed 2)
   renderPixelDirect hl pal (baseX + 3) y (extractPixel packed 3)
   renderPixelDirect hl pal (baseX + 4) y (extractPixel packed 4)
   renderPixelDirect hl pal (baseX + 5) y (extractPixel packed 5)
   renderPixelDirect hl pal (baseX + 6) y (extractPixel packed 6)
   renderPixelDirect hl pal (baseX + 7) y (extractPixel packed 7)

 -- Render sprite tile row: skips transparent (Col0) pixels and out-of-bounds
 {-# INLINE renderSpriteTileRow #-}
 renderSpriteTileRow :: Bool -> Palette -> Word16 -> Int -> Int -> GBC ()
 renderSpriteTileRow !hl !pal !packed !baseX !y = do
   renderSpritePixel hl pal packed baseX y 0
   renderSpritePixel hl pal packed baseX y 1
   renderSpritePixel hl pal packed baseX y 2
   renderSpritePixel hl pal packed baseX y 3
   renderSpritePixel hl pal packed baseX y 4
   renderSpritePixel hl pal packed baseX y 5
   renderSpritePixel hl pal packed baseX y 6
   renderSpritePixel hl pal packed baseX y 7

 {-# INLINE renderSpritePixel #-}
 renderSpritePixel :: Bool -> Palette -> Word16 -> Int -> Int -> Int -> GBC ()
 renderSpritePixel !hl !pal !packed !baseX !y !i = do
   let !px = baseX + i
       !col = extractPixel packed i
   when (col /= 0 && px >= 0 && px < 160) $
     renderPixelDirect hl pal px y col

 {-# INLINE toGBColor #-}
 toGBColor :: Word8 -> GBColor
 toGBColor 0 = Col0
 toGBColor 1 = Col1
 toGBColor 2 = Col2
 toGBColor _ = Col3


 {-# INLINE getGBShade #-}
 getGBShade :: GBColor -> Color
 getGBShade num = case num of
   Col0 -> white
   Col1 -> light_gray
   Col2 -> dark_gray
   Col3 -> black

 {-# INLINE getGBPalette #-}
 getGBPalette :: Word8 -> Palette
 getGBPalette pal = ((getGBShade col0), (getGBShade col1), (getGBShade col2), (getGBShade col3))
   where col0 = toGBColor $ pal .&. 0x3
         col1 = toGBColor $ (pal .&. 0xC) `shiftR` 2
         col2 = toGBColor $ (pal .&. 0x30) `shiftR` 4
         col3 = toGBColor $ (pal .&. 0xC0) `shiftR` 6

 --Bit 0-4   : Red Intensity
 --Bit 5-9   : Green Intensity
 --Bit 10-14 : Blue Intensity
 {-# INLINE getGBCColor #-}
 getGBCColor :: Word16 -> Color
 getGBCColor num =  Color 0xFF --A
                    (8 * (fromIntegral $ num .&. 0x1F)) --R
                    (8 * (fromIntegral $ (num `shiftR` 5) .&. 0x1F)) --G
                    (8 * (fromIntegral $ (num `shiftR` 10) .&. 0x1F)) --B

 {-# INLINE getGBCPalette #-}
 getGBCPalette :: Word8 -> GBC Palette
 getGBCPalette !num = do
     let !base = 8 * num
     !b0 <- readArr bgPalettes base
     !b1 <- readArr bgPalettes (base + 1)
     !b2 <- readArr bgPalettes (base + 2)
     !b3 <- readArr bgPalettes (base + 3)
     !b4 <- readArr bgPalettes (base + 4)
     !b5 <- readArr bgPalettes (base + 5)
     !b6 <- readArr bgPalettes (base + 6)
     !b7 <- readArr bgPalettes (base + 7)
     let !col0 = getGBCColor $ (fromIntegral b1 `shiftL` 8) .|. fromIntegral b0
     let !col1 = getGBCColor $ (fromIntegral b3 `shiftL` 8) .|. fromIntegral b2
     let !col2 = getGBCColor $ (fromIntegral b5 `shiftL` 8) .|. fromIntegral b4
     let !col3 = getGBCColor $ (fromIntegral b7 `shiftL` 8) .|. fromIntegral b6
     return (col0, col1, col2, col3)

 lcdc_addr :: Word16
 lcdc_addr  = 0xFF40

 stat_addr :: Word16
 stat_addr  = 0xFF41

 scy_addr :: Word16
 scy_addr   = 0xFF42

 scx_addr :: Word16
 scx_addr   = 0xFF43

 ly_addr :: Word16
 ly_addr    = 0xFF44

 lyc_addr :: Word16
 lyc_addr   = 0xFF45

 bgp_addr :: Word16
 bgp_addr   = 0xFF47

 hdma5_addr :: Word16
 hdma5_addr = 0xFF55

 renderWindow :: Bool -> Word8 -> GBC ()
 renderWindow !hl !line = do
   lcdc <- readArr memory lcdc_addr
   when (testBit lcdc 5) $ do  -- Window enable
     wy <- readArr memory 0xFF4A
     when (line >= wy) $ do
       wx <- readArr memory 0xFF4B
       let !wxAdj = fromIntegral wx - 7 :: Int
       gbc <- readRef gbc_mode
       let !(tileStart, offset) = if testBit lcdc 4 then (0x8000 :: Word16, 0) else (0x8800 :: Word16, 128)
       let !winStart = if testBit lcdc 6 then (0x9C00 :: Word16) else (0x9800 :: Word16)
       let !winY = line - wy
       let !yTileOffset = (fromIntegral (winY `div` 8) :: Word16) * 32
       let !yByteOff = 2 * fromIntegral (winY `mod` 8) :: Word16
       forM_ [0..20] $ \tileX -> do
         let !tileAddr = winStart + yTileOffset + tileX
         tileAttr <- if gbc then readArr vramBank tileAddr else return 0
         tileNum <- (+ offset) <$> readArr memory tileAddr
         let !yFlipped = if testBit tileAttr 6 then 2 * (7 - fromIntegral (winY `mod` 8)) else yByteOff
         let !finalAddr = fromIntegral tileNum * 16 + tileStart + yFlipped
         let tile_bank = if testBit tileAttr 3 then vramBank else memory
         lo <- readArr tile_bank finalAddr
         hi <- readArr tile_bank (finalAddr + 1)
         let !packed = if testBit tileAttr 5
                         then packTileRowFlipped lo hi
                         else packTileRow lo hi
         pal <- if gbc then getGBCPalette (tileAttr .&. 0x7) else getGBPalette <$> readArr memory bgp_addr
         let !baseX = wxAdj + fromIntegral tileX * 8
         renderTileRow hl pal packed baseX (fromIntegral line)

 renderSprites :: Bool -> Word8 -> GBC ()
 renderSprites !hl !line = do
   lcdc <- readArr memory lcdc_addr
   when (testBit lcdc 1) $ do  -- Sprite enable
     let !spriteHeight = if testBit lcdc 2 then 16 else 8 :: Int
     -- Scan OAM for sprites on this scanline (max 10 per line)
     sprites <- foldM (\acc i -> do
       let !oamAddr = 0xFE00 + i * 4
       yPos <- readArr memory oamAddr
       let !spriteY = fromIntegral yPos - 16 :: Int
           !lineI = fromIntegral line :: Int
       if lineI >= spriteY && lineI < spriteY + spriteHeight && length acc < 10
         then do
           xPos <- readArr memory (oamAddr + 1)
           tileN <- readArr memory (oamAddr + 2)
           attrs <- readArr memory (oamAddr + 3)
           return ((xPos, spriteY, tileN, attrs) : acc)
         else return acc
       ) [] [0..39 :: Word16]
     -- Render sprites in reverse order (lower OAM index = higher priority, drawn last)
     forM_ sprites $ \(xPos, spriteY, tileN, attrs) -> do
       let !spriteX = fromIntegral xPos - 8 :: Int
           !lineI = fromIntegral line :: Int
           !yFlip = testBit attrs 6
           !xFlip = testBit attrs 5
           -- For 8x16 mode, mask off bit 0 of tile number
           !tileNum = if spriteHeight == 16 then tileN .&. 0xFE else tileN
           !row = lineI - spriteY
           !effectiveRow = if yFlip then spriteHeight - 1 - row else row
           !tileAddr = 0x8000 + fromIntegral tileNum * 16 + fromIntegral effectiveRow * 2 :: Word16
       lo <- readArr memory tileAddr
       hi <- readArr memory (tileAddr + 1)
       let !packed = if xFlip
                       then packTileRowFlipped lo hi
                       else packTileRow lo hi
       -- Use OBP0 or OBP1 based on bit 4
       pal <- getGBPalette <$> readArr memory (if testBit attrs 4 then 0xFF49 else 0xFF48)
       renderSpriteTileRow hl pal packed spriteX (fromIntegral line)
