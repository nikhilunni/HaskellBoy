module Cartridge
       ( readCartridge
       , storeCartridge) where

 import qualified Data.ByteString as C

 import Control.Applicative
 import Data.Word

 import Monad
 import Memory

 readCartridge :: FilePath -> IO [Word8]
 readCartridge fpath = C.unpack <$> C.readFile fpath

 storeCartridge :: (Emulator m) => [Word8] -> m ()
 storeCartridge ws = mapM_ (\(c,i) -> store (MemAddr i) (MemVal8 c)) $ zip ws [0..]
