 module Monad
        where

 import Types
 import Memory 
 import Data.Word (Word8, Word16)
 
 class (Functor m, Monad m) => Emulator m where
   load :: Address -> m MemVal
   store :: Address -> MemVal -> m ()
   emulationError :: String -> m ()
   printM :: String -> m ()
   pause :: m ()
   drawPixel :: Pixel -> m ()
   showScreen :: m ()
