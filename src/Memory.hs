module Memory 
 where
       
 import Control.Applicative
 import Control.Monad.ST
 import Data.Word
 import Data.Array.ST
 import Data.STRef
 import Data.Bits

 data Register =   A
                 | F
                 | B
                 | C
                 | D
                 | E
                 | H
                 | L
                 deriving (Show, Enum)

 data Address = OneRegister Register
              | TwoRegister {registerA :: Register, registerB :: Register} --Enforce pairings? (BC, DE, HL)
              | MemAddr Word16
              | SP
              | PC
              deriving (Show)

 data MemVal = MemVal8 Word8
             | MemVal16 Word16
              deriving (Show)

 data Memory s = Memory { memory :: STUArray s Word16 Word8
                        , registers :: STUArray s Word8 Word8
                        , sp :: STRef s Word16
                        , pc :: STRef s Word16
                        }                          


 new :: ST s (Memory s)
 new = do
   memory' <- newArray_ (0x0000, 0xFFFF)
   registers' <- newArray (0x0, 0x8) 0 --Fix this...
   sp' <- newSTRef 0xFFFE
   pc' <- newSTRef 0x0000
   return Memory { memory = memory'
                 , registers = registers'
                 , sp = sp'
                 , pc = pc'
                 }

 regNum = fromIntegral . fromEnum
       
 read :: Memory s -> Address -> ST s MemVal
 read mem (OneRegister reg)       = readArray (registers mem) (regNum reg) >>= \n -> return $ MemVal8 n
 read mem (TwoRegister regA regB) =
   do
   a <- readArray (registers mem) (regNum regA)
   b <- readArray (registers mem) (regNum regB)
   return $ MemVal16 $ fromIntegral $ (a `shiftL` 8) + (b) 
 read mem (MemAddr ptr)           = readArray (memory mem) ptr >>= \n -> return $ MemVal8 n
 read mem SP                      = readSTRef (sp mem) >>= \n -> return $ MemVal16 n
 read mem PC                      = readSTRef (pc mem) >>= \n -> return $ MemVal16 n



 write :: Memory s -> Address -> MemVal -> ST s ()
 write mem (OneRegister reg) (MemVal8 w)        = writeArray (registers mem) (regNum reg) w
 write mem (TwoRegister regA regB) (MemVal16 w) =
   do
   writeArray (registers mem) (regNum regA) $ fromIntegral (w `shiftR` 8)
   writeArray (registers mem) (regNum regB) $ fromIntegral (w .&. 0xFF)
 write mem (MemAddr ptr) (MemVal8 w)            = writeArray (memory mem) ptr w
 write mem SP (MemVal16 w) = writeSTRef (sp mem) w
-- write mem PC (MemVal16 w) = writeSTRef (pc mem) w
