{-# LANGUAGE GeneralizedNewtypeDeriving #-} --Unsafe!
module GBC
 where

 import Control.Monad.Reader
 import Control.Monad.ST
 import Control.Monad.Trans
 import Control.Applicative

 import Memory
 import Monad

 newtype GBC a = GBC (ReaderT (Memory RealWorld) IO a)
                 deriving (Functor, Applicative, Monad, MonadIO)
 
 instance Emulator GBC where
   load address = GBC $ do
     mem <- ask
     lift $ stToIO $ Memory.read mem address
   store address val = GBC $ do
     mem <- ask
     lift $ stToIO $ Memory.write mem address val



 runGBC :: GBC a -> IO a
 runGBC (GBC reader) = do
   mem <- stToIO Memory.new
   runReaderT reader mem


 test :: Emulator m => m MemVal
 test = do
   store (OneRegister A) (MemVal8 2)
   load (OneRegister A)

 main :: IO ()
 main = runGBC $ do
           v <- test
           liftIO $ putStrLn $ show v
