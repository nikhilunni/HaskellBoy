{-# LANGUAGE TemplateHaskell #-}

module Memory 
 where

 import Control.Applicative
 import Control.Monad.ST
 import Data.Word
 import Data.Array.ST
 import Data.STRef
 import Data.Bits

 import TemplateMemory
 import Types

 import SDL

 new :: Window -> Renderer -> ST s (Memory s)
 new window' renderer' = do
   memory' <- newArray_ (0x0000, 0xFFFF)
   vramBank' <- newArray_ (0x8000, 0x9FFF)
   registers' <- newArray (0x0, 0x8) 0
   sp' <-  newSTRef 0xFFFE
   pc' <-  newSTRef 0x0100
   cycles' <- newSTRef 0
   ime' <- newSTRef False
   halt' <- newSTRef False
   mode' <- newSTRef OAMSearch
   line' <- newSTRef 0
   transferred' <- newSTRef False
   gpu_cycles' <- newSTRef 0
   gbc_mode' <- newSTRef True
   return Memory { memory = memory'
                 , vramBank = vramBank'
                 , registers = registers'
                 , memRefs = MemRefs { sp = sp'
                                     , pc = pc'
                                     , cycles = cycles'
                                     , ime = ime'
                                     , halt = halt'
                                     , mode = mode'
                                     , line = line'
                                     , transferred = transferred'
                                     , gpu_cycles = gpu_cycles'
                                     , gbc_mode = gbc_mode'
                                     }
                 , window = window'
                 , renderer = renderer'
                 }

 build_address_type ''MemRefs
 build_read_write ''MemRefs
 

 regNum = fromIntegral . fromEnum
       
 read :: Memory s -> Address -> ST s MemVal
 read mem (OneRegister reg)       = readArray (registers mem) (regNum reg) >>= \n -> return $ MemVal8 n
 read mem (TwoRegister regA regB) = do a <- readArray (registers mem) (regNum regA)
                                       b <- readArray (registers mem) (regNum regB)
                                       return $ MemVal16 $ fromIntegral $ (a `shiftL` 8) + (b)
 read mem (MemAddr ptr)           = readArray (memory mem) ptr >>= \n -> return $ MemVal8 n
 read mem (VRAMAddr ptr)          = readArray (vramBank mem) ptr >>= \n -> return $ MemVal8 n
 read mem other                   = readAccess mem other


 write :: Memory s -> Address -> MemVal -> ST s ()
 write mem (OneRegister reg) (MemVal8 w)        = writeArray (registers mem) (regNum reg) w
 write mem (TwoRegister regA regB) (MemVal16 w) =
   do
   writeArray (registers mem) (regNum regA) $ fromIntegral (w `shiftR` 8)
   writeArray (registers mem) (regNum regB) $ fromIntegral (w .&. 0xFF)
 write mem (MemAddr ptr) (MemVal8 w)     = writeArray (memory mem) ptr w
 write mem (VRAMAddr ptr) (MemVal8 w)    = writeArray (memory mem) ptr w
 write mem other val                     = writeAccess mem other val
