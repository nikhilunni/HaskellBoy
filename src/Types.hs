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


 import Language.Haskell.TH

 import SDL hiding (Palette)


 data Register = A
               | F
               | B
               | C
               | D
               | E
               | H
               | L
               deriving (Show, Enum)
                          
 data MemVal = MemVal8 Word8
             | MemVal16 Word16
             | Flag Bool
             | Mode GPUMode
              deriving (Show)

 data GPUMode = HBlankMode | VBlankMode | OAMSearch | Transfer deriving (Enum, Show)


 data Memory s = Memory { memory :: STUArray s Word16 Word8
                        , vramBank :: STUArray s Word16 Word8
                        , bgPalettes :: STArray s Word8 Word8
                        , registers :: STUArray s Word8 Word8
                        , memRefs :: MemRefs s
                        , window :: Window
                        , renderer :: Renderer
                        }
                 
 data MemRefs s = MemRefs { sp :: STRef s Word16
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


--TODO : Convert from STArray to STUArray!

{-
instance MArray (STUArray s) Palette (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (\(c1, c2, c3, c4) -> (c1) + (c2 `shiftL` 8) + (c3 `shiftL` 16) + (c4 `shiftL` 24))
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = newArray arrBounds 0
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = ST $ \s1# ->
        case readWord8Array# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, W8# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) (W8# e#) = ST $ \s1# ->
        case writeWord8Array# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }
-}

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

 
 
