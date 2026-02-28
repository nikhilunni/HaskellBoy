{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
 where

 import Data.Word
 import Data.Array.ST
 import Data.STRef

 import Control.Monad.ST (RealWorld)
 import Control.Monad.Reader

 import SDL hiding (Palette)


 newtype GBC a = GBC (ReaderT (Memory RealWorld) IO a)
               deriving (Functor, Applicative, Monad, MonadIO)

 data Register = A
               | F
               | B
               | C
               | D
               | E
               | H
               | L
               deriving (Show, Enum)

 eNum :: (Enum a, Num c) => a -> c
 eNum = fromIntegral . fromEnum

 data GPUMode = HBlankMode | VBlankMode | OAMSearch | Transfer deriving (Enum, Show)

 data Memory s = Memory { memory :: STUArray s Word16 Word8
                        , bgPalettes :: STArray s Word8 Word8
                        , registers :: STUArray s Word8 Word8
                        , romBanks  :: STUArray s (Word8,Word16) Word8
                        , vramBank :: STUArray s Word16 Word8
                        , wramBanks :: STUArray s (Word8,Word16) Word8
                        , window :: Window
                        , renderer :: Renderer
                        , sp :: STRef s Word16
                        , pc :: STRef s Word16
                        , cycles :: STRef s Word16
                        , ime :: STRef s Bool --Interrupt Master Enable Flag
                        , imePending :: STRef s Bool --EI delayed enable
                        , halt :: STRef s Bool --Are we halted or not
                        , mode :: STRef s GPUMode -- GPU mode
                        , line :: STRef s Word8 -- GPU line
                        , transferred :: STRef s Bool --Have we done GPU scanline?
                        , gpu_cycles :: STRef s Word16 -- Internal cycle count for GPU
                        , gbc_mode :: STRef s Bool --Are we in GBC mode?
                        , bootRomActive :: STRef s Bool --Is boot ROM overlaid?
                        , cartFirstPage :: STUArray s Word16 Word8 --Saved cartridge 0x00-0xFF
                        , traceEnabled :: STRef s Bool --Instruction tracing
                        , romBank :: STRef s Word8 --Current switchable ROM bank
                        , framebuffer :: STUArray s Int Word8
                        , screenshotDir :: STRef s (Maybe FilePath)
                        , testMode :: STRef s Bool --Flat 64K access for CPU tests
                        , headless :: STRef s Bool --Skip SDL rendering
                        , serialBuffer :: STRef s String --Accumulated serial output
                        , timerCounter :: STRef s Word16 --Internal timer cycle counter
                        , divCounter :: STRef s Word16 --Internal DIV cycle counter
                        }

 data CartMode = DMGMode | GBCMode

 data MBCType = MBC0
              | MBC1
              | MBC2
              | MBC3
              | MBC4
              | MBC5
              | HuC1

 data Color = Color Word8 Word8 Word8 Word8 deriving Show --A/R/G/B
 data Pixel = Pixel Color Integer Integer deriving Show -- Color/X/Y
 type Palette = (Color, Color, Color, Color) --Color 0,1,2,3
 data GBColor = Col0 | Col1 | Col2 | Col3

 white :: Color
 white      = Color 0xFF 0xFF 0xFF 0xFF

 light_gray :: Color
 light_gray = Color 0xFF 0xE0 0xE0 0xE0

 dark_gray :: Color
 dark_gray  = Color 0xFF 0x66 0x66 0x66

 black :: Color
 black      = Color 0xFF 0x00 0x00 0x00

 type Opcode = Word8
 type Cycles = Word16
 type LineNum = Word8

 fst3 :: (a, b, c) -> a
 fst3 (a,_,_) = a

 snd3 :: (a, b, c) -> b
 snd3 (_,b,_) = b

 thd3 :: (a, b, c) -> c
 thd3 (_,_,c) = c

 lift8 :: GBC Word8 -> GBC Word16
 lift8 = fmap ((+0xFF00) . fromIntegral)
