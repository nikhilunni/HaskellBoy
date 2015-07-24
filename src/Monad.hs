 module Monad
        (
          Emulator (..)
        ) where

 import Data.Bits (bitSizeMaybe)

 class (Eq a, Show a) => Address a where
   

 class (Functor m, Monad m) => Emulator where
   load :: (Address a, Value v) => a -> m v
   store :: (Address a, Value m) => a -> v -> m ()
   draw :: m ()
   run :: m ()
 
 
