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
 import MemoryRules
 import Data.Word (Word8, Word16)

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
