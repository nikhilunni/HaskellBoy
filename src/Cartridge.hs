module Cartridge
       ( readCartridge
       , storeCartridge) where

 import qualified Data.ByteString as C

 import Control.Applicative
 import Data.Word

 import Types
 import Monad
 import Memory
 import MemoryRules

 readCartridge :: FilePath -> IO [Word8]
 readCartridge fpath = C.unpack <$> C.readFile fpath

 storeCartridge :: [Word8] -> GBC ()
 storeCartridge ws = do
   mapM_ (\(c,i) -> writeMem i c) $ zip ws [0..]
