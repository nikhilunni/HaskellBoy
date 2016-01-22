module Types
 where

 import Data.Word
 import Data.Bits
 import Data.Char
 import Data.Array.ST
 import Data.STRef

 import Control.Monad
 import Control.Monad.ST


 import Language.Haskell.TH

 import SDL


 data Register =   A
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
                        , bgp :: STUArray s 
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


 data Color = Color Word8 Word8 Word8 Word8 --A/R/G/B
 data Pixel = Pixel Color Integer Integer -- Color/X/Y

 type Opcode = Word8
 type Cycles = Word16  

 fst3 (a,_,_) = a
 snd3 (_,b,_) = b
 thd3 (_,_,c) = c

 
 
