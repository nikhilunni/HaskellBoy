{-# LANGUAGE TemplateHaskell #-}

module GPU
 where


 import Types
 import Monad 
 import Memory
 import CPU

 import Control.Applicative
 import Control.Monad
 import Control.Monad.ST
 import Control.Monad.Trans
 import Data.Word
 import Data.Array.ST
 import Data.STRef
 import Data.Bits

 lcdc_addr  = MemAddr 0xFF40
 stat_addr  = MemAddr 0xFF41
 scy_addr   = MemAddr 0xFF42
 scx_addr   = MemAddr 0xFF43
 ly_addr    = MemAddr 0xFF44
 lyc_addr   = MemAddr 0xFF45
 bgp_addr   = MemAddr 0xFF47
 hdma5_addr = MemAddr 0xFF55

 screen_width  = 160
 screen_height = 144
 
 compareLY_LYC :: GBC ()  --TODO : Implement in memory access "rules" logic
 compareLY_LYC = do
   MemVal8 ly <- load ly_addr
   MemVal8 lyc <- load lyc_addr
   MemVal8 stat <- load stat_addr
   store stat_addr $ MemVal8 $ (if ly == lyc then setBit else clearBit) stat 2
   if (ly == lyc && isBitSet stat 6) then requestInterrupt STAT else return () --Convert to when (?)
     
     
 --TODO : Eliminate GPU_MODE memory altogether... just store in STAT (?)
 lcdModeSwitch :: GPUMode -> GBC ()
 lcdModeSwitch m = do
   store MODE (Mode m)
   MemVal8 stat <- load stat_addr
   store stat_addr $ MemVal8 $ stat .|. (fromIntegral.fromEnum $ m)
   when (isBitSet stat ( (+3).fromIntegral.fromEnum $ m)) $ do
     requestInterrupt STAT


 hdmaEnabled :: GBC Bool
 hdmaEnabled = do
   MemVal8 hdma5 <- load hdma5_addr
   return $ hdma5 >= 128 --HDMA is enabled if the 7th bit is set (hdma5 `shiftL` 7 == 1)
   
 --Imperative af :-(
 tick :: Cycles -> GBC ()
 tick delta = do
   MemVal8 lineNum <- load LINE
   Mode scanMode <- load MODE
   MemVal16 counter <- load GPU_CYCLES
   let total = counter + delta
   store GPU_CYCLES (MemVal16 total)
   case scanMode of
     HBlankMode -> do
       when (total >= 456) $ do --End of HBlank period
         store GPU_CYCLES (MemVal16 $ total - 456)
         store LINE (MemVal8 $ lineNum + 1)
         store ly_addr (MemVal8 $ lineNum + 1)
         compareLY_LYC
         
         --TODO : HDMA transfer
         if (lineNum + 1 == 144)
           then do requestInterrupt VBlank
                   lcdModeSwitch VBlankMode
           else do lcdModeSwitch OAMSearch
                   
     VBlankMode -> do
       if (lineNum + 1 < 153)
          then do when (total >= 456) $ do --End of line
                    store GPU_CYCLES (MemVal16 $ total - 456)
                    store LINE (MemVal8 $ lineNum + 1)
                    store ly_addr (MemVal8 $ lineNum + 1)
                    compareLY_LYC
          else do MemVal8 ly <- load ly_addr
                  when (total >= 8 && ly == 153) $ do
                    store ly_addr (MemVal8 0) --LY value is reset early, for whatever reason...
                    compareLY_LYC
                  when (total >= 456) $ do --Last line of VBlank is over
                    store GPU_CYCLES (MemVal16 $ total - 456)
                    store LINE (MemVal8 0)
                    lcdModeSwitch OAMSearch
                    
     OAMSearch -> when (total >= 80) $ (store TRANSFERRED $ Flag False) >> lcdModeSwitch Transfer
     Transfer -> do
       Flag scanned <- load TRANSFERRED
       MemVal8 ly <- load ly_addr
       when (not scanned && total >= (if ly == 0 then 160 else 48)) $ do
         store TRANSFERRED (Flag True)
         scanLine lineNum
       when (total >= 172) $ lcdModeSwitch HBlankMode



 scanLine :: Word8 -> GBC ()
 scanLine lineNum = do
   MemVal8 lcdc <- load lcdc_addr
   if (isBitSet lcdc 7)
      then renderLine lineNum
     else return ()

 renderLine :: Word8 -> GBC ()
 renderLine line = do
   renderBackground line
   renderWindow line
   renderSprites line


 renderBackground :: Word8 -> GBC ()
 renderBackground line = do
   MemVal8 lcdc <- load lcdc_addr
   Flag gbc <- load GBC_MODE
   if (isBitSet lcdc 0) --If BG display is enabled
      then do let (tileStart, offset) = if isBitSet lcdc 4 then (0x8000::Word16, 0) else (0x8800::Word16, 128)
              let bgStart = if isBitSet lcdc 3 then (0x9C00::Word16) else (0x9800::Word16)
              MemVal8 scy <- load scy_addr
              MemVal8 scx <- load scx_addr

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
                MemVal8 tileAttr <- if gbc then (load $ VRAMAddr tileIdx) else (return $ MemVal8 0)
                tileNum <- (\(MemVal8 a) -> a + offset) <$> (load $ MemAddr tileIdx)

                let yByteOffset = 2 * (if isBitSet tileAttr 6 then (7 - (totalY `mod` 8)) else (totalY `mod` 8))
                let finalTileAddr = (fromIntegral $ tileNum * 16) + tileStart + (fromIntegral yByteOffset)
                let tile_bank = (if isBitSet tileAttr 3 then MemAddr else VRAMAddr)
                MemVal8 lowerByte <- load $ tile_bank finalTileAddr
                MemVal8 upperByte <- load $ tile_bank $ finalTileAddr + 1
                let pixelNums = (if isBitSet tileAttr 5 then reverse else id) $ getPixelNums lowerByte upperByte
                let posX = [ fromIntegral ((tileIdx - tileAddrOffsetLower - 1) * 8 - fromIntegral scx) ..
                             fromIntegral ((tileIdx - tileAddrOffsetLower) * 8 - 1 - fromIntegral scx) ]
                pal <- (if gbc then (getGBCPalette $ tileAttr .&. 0x7) else (getGBPalette <$> unpackMemVal8 <$> load bgp_addr))
                runList $ [renderPixel pal posX scy col | (posX,col) <- zip posX pixelNums]
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

 --TODO : Check for GBP #0-7, otherwise error ... right now you'll just get a runtime exception for index out of bound
 getGBCPalette :: Word8 -> GBC Palette
 getGBCPalette num = do
     MemVal8 pal <- load $ PaletteAddr $ 8 * num
     bytes <- getBytes <$> sequence [load $ PaletteAddr $ 8 * num + i | i <- [0 .. 7]]
     let col0 = getGBCColor $ (fromIntegral $ ((bytes !! 0) `shiftL` 8)) + (fromIntegral $ (bytes !! 1))
     let col1 = getGBCColor $ (fromIntegral $ ((bytes !! 2) `shiftL` 8)) + (fromIntegral $ (bytes !! 3))
     let col2 = getGBCColor $ (fromIntegral $ ((bytes !! 4) `shiftL` 8)) + (fromIntegral $ (bytes !! 5))
     let col3 = getGBCColor $ (fromIntegral $ ((bytes !! 6) `shiftL` 8)) + (fromIntegral $ (bytes !! 7))
     return (col0, col1, col2, col3)
   where getBytes [] = []
         getBytes (MemVal8 x:xs) = x : (getBytes xs)

 unpackMemVal8 :: MemVal -> Word8
 unpackMemVal8 (MemVal8 x) = x

   
 renderWindow line = return ()
 renderSprites line = return ()
