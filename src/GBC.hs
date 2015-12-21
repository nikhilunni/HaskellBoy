{-# LANGUAGE GeneralizedNewtypeDeriving #-} --Unsafe!

module GBC
 where

 import Control.Monad.Reader
 import Control.Monad.ST
 import Control.Monad.Trans
 import Control.Applicative

 import Memory
 import Monad
 import Cartridge

 import System.Exit
 import System.IO 

 newtype GBC a = GBC (ReaderT (Memory RealWorld) IO a)
                 deriving (Functor, Applicative, Monad, MonadIO)
 
 instance Emulator GBC where
   load address = GBC $ do
     mem <- ask
     lift $ stToIO $ Memory.read mem address
   store address val = GBC $ do
     mem <- ask
     lift $ stToIO $ Memory.write mem address val
   emulationError msg = GBC $ do
     lift $ hPutStrLn stderr msg >> exitFailure


 runGBC :: GBC a -> IO a
 runGBC (GBC reader) = do
   mem <- stToIO Memory.new
   runReaderT reader mem


 test :: Emulator m => m MemVal
 test = do
   store (OneRegister A) (MemVal8 2)
   load (OneRegister A)

 main :: IO ()
 main = do
   cart <- readCartridge "CPU.hs"
   runGBC $ do
     storeCartridge cart
     MemVal8 thing <- load (MemAddr 0x0000)
     liftIO $ putStrLn $ show $ thing
