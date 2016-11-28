{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
 where

 import Data.Word
 import Data.Bits
 import Data.Char
 import Data.Array.ST
 import Data.STRef
 import Data.Either

 import Control.Monad
 import Control.Monad.ST
 import Control.Monad.Reader
 import Control.Monad.Trans

 import Control.Lens


 import Language.Haskell.TH

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
                        , halt :: STRef s Bool --Are we halted or not
                        , mode :: STRef s GPUMode -- GPU mode
                        , line :: STRef s Word8 -- GPU line
                        , transferred :: STRef s Bool --Have we done GPU scanline?
                        , gpu_cycles :: STRef s Word16 -- Internal cycle count for GPU
                        , gbc_mode :: STRef s Bool --Are we in GBC mode?
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

 white      = Color 0xFF 0xFF 0xFF 0xFF
 light_gray = Color 0xFF 0xE0 0xE0 0xE0
 dark_gray  = Color 0xFF 0x66 0x66 0x66
 black      = Color 0xFF 0x00 0x00 0x00

 type Opcode = Word8
 type Cycles = Word16
 type LineNum = Word8

 fst3 (a,_,_) = a
 snd3 (_,b,_) = b
 thd3 (_,_,c) = c

 lift8 :: GBC Word8 -> GBC Word16
 lift8 = fmap ((+0xFF00) . fromIntegral)
