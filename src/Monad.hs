{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad
        where

 import Control.Monad
 import Control.Monad.Reader
 import Control.Monad.ST
 import Control.Monad.Trans
 import Control.Applicative

 import SDL
 
 import Linear (V4(..), V2(..))
 import Linear.Affine

 import Foreign.C.Types

 
 import System.Exit 
 import System.IO
 
 import Types
 import Memory
 import Data.Word (Word8, Word16)


 newtype GBC a = GBC (ReaderT (Memory RealWorld) IO a)
               deriving (Functor, Applicative, Monad, MonadIO)

 load :: Address -> GBC MemVal
 load address = GBC $ do
   mem <- ask
   lift $ stToIO $ Memory.read mem address
   
 store :: Address -> MemVal -> GBC ()
 store address val = GBC $ do
   mem <- ask
   lift $ stToIO $ Memory.write mem address val

 emulationError :: String -> GBC ()   
 emulationError msg = GBC $ do
   lift $ hPutStrLn stderr msg >> exitFailure

 printM :: String -> GBC ()
 printM msg = GBC $ do
   lift $ putStrLn msg

 pause :: GBC () 
 pause = GBC $ do
   lift $ getLine >> return ()

 drawPixel :: Pixel -> GBC ()   
 drawPixel pix@(Pixel col@(Color a r g b) x y) = GBC $ do
   mem <- ask
   rendererDrawColor (renderer mem) $= V4 a r g b
   drawPoint (renderer mem) (P $ V2 (CInt $ fromIntegral x) (CInt $ fromIntegral y))

 showScreen :: GBC ()   
 showScreen = GBC $ do
   mem <- ask
   present (renderer mem)
 
 runList :: (Foldable t, Monad m) => t (m ()) -> m ()
 runList = foldl (>>) (return ())
