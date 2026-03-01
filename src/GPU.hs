module GPU
 where

 import Control.Monad
 import Data.Word
 import Data.Bits

 import Types
 import Monad
 import Memory


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

 compareLY_LYC :: GBC ()
 compareLY_LYC = do
   ly <- readArr memory ly_addr
   lyc <- readArr memory lyc_addr
   stat <- readArr memory stat_addr
   writeArr memory stat_addr $ (if ly == lyc then setBit else clearBit) stat 2
   when (ly == lyc && testBit stat 6) $ requestInterrupt STAT


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



 scanLine :: Word8 -> GBC ()
 scanLine lineNum = do
   lcdc <- readArr memory lcdc_addr
   if (testBit lcdc 7)
      then renderLine lineNum
     else return ()

 renderLine :: Word8 -> GBC ()
 renderLine line = do
   renderBackground line
   renderWindow line
   renderSprites line


 renderBackground :: Word8 -> GBC ()
 renderBackground line = do
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

              forM [tileAddrOffsetLower .. tileAddrOffsetUpper] $ \tileIdx -> do
                tileAttr <- if gbc then (readArr vramBank tileIdx) else (return 0)
                tileNum <- (+ offset) <$> (readArr memory tileIdx)

                let yByteOffset = 2 * (if testBit tileAttr 6 then (7 - (totalY `mod` 8)) else (totalY `mod` 8))
                let finalTileAddr = (fromIntegral tileNum * 16) + tileStart + (fromIntegral yByteOffset)
                let tile_bank = (if testBit tileAttr 3 then vramBank else memory)
                lowerByte <- readArr tile_bank finalTileAddr
                upperByte <- readArr tile_bank $ finalTileAddr + 1
                let pixelNums = (if testBit tileAttr 5 then reverse else id) $ getPixelNums lowerByte upperByte
                let posXs = [ fromIntegral ((tileIdx - tileAddrOffsetLower - 1) * 8 - fromIntegral scx) ..
                             fromIntegral ((tileIdx - tileAddrOffsetLower) * 8 - 1 - fromIntegral scx) ]
                pal <- (if gbc then (getGBCPalette $ tileAttr .&. 0x7) else (getGBPalette <$> readArr memory bgp_addr))
                sequence_ $ [renderPixel pal px line col | (px,col) <- zip posXs pixelNums]
              return ()
     else return ()

 --Lower byte represents LSB, upper byte represents MSB of each 8 pixels, with range [0 .. 3]
 getPixelNums :: Word8 -> Word8 -> [GBColor]
 getPixelNums lowerByte upperByte = [toGBColor $ (if lowerByte `testBit` i then 1 else 0) +
                                     (if upperByte `testBit` i then 2 else 0) | i <- [7,6 .. 0]]

 toGBColor :: Word8 -> GBColor
 toGBColor 0 = Col0
 toGBColor 1 = Col1
 toGBColor 2 = Col2
 toGBColor 3 = Col3

 renderPixel :: Palette -> Word8 -> Word8 -> GBColor -> GBC ()
 renderPixel pal posx posy gbcol = drawPixel $ Pixel (getColor pal gbcol) (fromIntegral posx) (fromIntegral posy)

 getColor :: Palette -> GBColor -> Color
 getColor (col0,col1,col2,col3) pixelNum = case pixelNum of
   Col0 -> col0
   Col1 -> col1
   Col2 -> col2
   Col3 -> col3


 getGBShade :: GBColor -> Color
 getGBShade num = case num of
   Col0 -> white
   Col1 -> light_gray
   Col2 -> dark_gray
   Col3 -> black

 getGBPalette :: Word8 -> Palette
 getGBPalette pal = ((getGBShade col0), (getGBShade col1), (getGBShade col2), (getGBShade col3))
   where col0 = toGBColor $ pal .&. 0x3
         col1 = toGBColor $ (pal .&. 0xC) `shiftR` 2
         col2 = toGBColor $ (pal .&. 0x30) `shiftR` 4
         col3 = toGBColor $ (pal .&. 0xC0) `shiftR` 6

 --Bit 0-4   : Red Intensity
 --Bit 5-9   : Green Intensity
 --Bit 10-14 : Blue Intensity
 getGBCColor :: Word16 -> Color
 getGBCColor num =  Color 0xFF --A
                    (8 * (fromIntegral $ num .&. 0x1F)) --R
                    (8 * (fromIntegral $ (num `shiftR` 5) .&. 0x1F)) --G
                    (8 * (fromIntegral $ (num `shiftR` 10) .&. 0x1F)) --B

 getGBCPalette :: Word8 -> GBC Palette
 getGBCPalette num = do
     let base = 8 * num
     bytes <- sequence [readArr bgPalettes (base + i) | i <- [0 .. 7]]
     let col0 = getGBCColor $ (fromIntegral (bytes !! 1) `shiftL` 8) .|. fromIntegral (bytes !! 0)
     let col1 = getGBCColor $ (fromIntegral (bytes !! 3) `shiftL` 8) .|. fromIntegral (bytes !! 2)
     let col2 = getGBCColor $ (fromIntegral (bytes !! 5) `shiftL` 8) .|. fromIntegral (bytes !! 4)
     let col3 = getGBCColor $ (fromIntegral (bytes !! 7) `shiftL` 8) .|. fromIntegral (bytes !! 6)
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

 renderWindow line = return ()
 renderSprites line = return ()
