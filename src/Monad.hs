 module Monad
        (
          Emulator (..)
        ) where

 import Memory (Address, MemVal)

 class (Functor m, Monad m) => Emulator m where
   load :: Address -> m MemVal
   store :: Address -> MemVal -> m ()
 
