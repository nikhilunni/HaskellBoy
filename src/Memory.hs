module Memory 
         ( Memory
         )
 where
       
 import Control.Applicative
 import Control.Monad.ST
 import Data.Word
 import Data.STRef
 import Data.Monoid

 type Mem8 s  = (STRef s Word8)
 type Mem16 s = (STRef s Word16)

--Use Register for CPU Opcode instructions, have a separate Ref for each register
--Don't combile HL -- Have a ref for H and have a ref for L
 data Memory = Register (STRef s Word8)
             | Register (STRef s Word8) (STRef s Word8)
             | Ram (STRef s Word16)

 data MemSize = Word8 | Word16

 data RegisterSet s = RegisterSet { a  :: {-# UNPACK #-} !(Mem8  s)
                                  , f  :: {-# UNPACK #-} !(Mem8  s)
                                  , b  :: {-# UNPACK #-} !(Mem8  s)
                                  , c  :: {-# UNPACK #-} !(Mem8  s)
                                  , d  :: {-# UNPACK #-} !(Mem8  s)
                                  , e  :: {-# UNPACK #-} !(Mem8  s)
                                  , h  :: {-# UNPACK #-} !(Mem8  s)
                                  , l  :: {-# UNPACK #-} !(Mem8  s)
                                  , af :: {-# UNPACK #-} !(Mem16 s)
                                  , bc :: {-# UNPACK #-} !(Mem16 s)
                                  , de :: {-# UNPACK #-} !(Mem16 s)
                                  , hl :: {-# UNPACK #-} !(Mem16 s)
                                  , sp :: {-# UNPACK #-} !(Mem16 s)
                                  , pc :: {-# UNPACK #-} !(Mem16 s)
                                  }


 data Register =   A
                 | F
                 | B
                 | C
                 | D
                 | E
                 | H
                 | L
                 | AF
                 | BC
                 | DE
                 | HL
                 | SP
                 | PC


 new :: ST s (Memory s)
 new = runST $ do
   
       

 
 getRegisterRef :: Register -> RegisterSet s -> STRef s Word8
 getRegisterRef reg set = case reg of
     A  -> a $ set
     F  -> f $ set
     B  -> b $ set
     C  -> c $ set
     D  -> d $ set
     E  -> e $ set
     H  -> h $ set
     L  -> l $ set
     AF -> af $ set
     BC -> bc $ set
     DE -> de $ set
     HL -> hl $ set
     SP -> sp $ set
     PC -> pc $ set
 

 initRegisters :: ST s (RegisterSet s)
 initRegisters = let k8 = newSTRef  (0 :: Word8)
                     k16 = newSTRef (0 :: Word16)
                 in RegisterSet
    <$> k8  <*> k8  <*> k8  <*> k8  <*> k8
    <*> k8  <*> k8  <*> k8  <*> k16 <*> k16
    <*> k16 <*> k16 <*> k16 <*> k16

