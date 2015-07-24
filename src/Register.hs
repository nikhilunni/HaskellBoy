module Register 
         ( Register
         )
 where
       
 import Control.Applicative
 import Control.Monad.ST
 import Data.Word
 import Data.STRef
 import Data.Monoid

 type Mem8 s  = STRef s Word8
 type Mem16 s = STRef s Word16 
 

 data Register s = Register { a  :: {-# UNPACK #-} !(Mem8  s)
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

 

 initRegisters :: ST s (Register s)
 initRegisters = let k8 = newSTRef  (0 :: Word8)
                     k16 = newSTRef (0 :: Word16)
                 in Register
    <$> k8  <*> k8  <*> k8  <*> k8  <*> k8
    <*> k8  <*> k8  <*> k8  <*> k16 <*> k16
    <*> k16 <*> k16 <*> k16 <*> k16

