{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module GBC
 where

 import Control.Monad
 import Control.Monad.Reader
 import Control.Monad.ST
 import Control.Monad.Trans
 import Control.Applicative

 import Data.Word (Word8, Word16)
 import Data.STRef

 import Types
 import Memory
 import Monad
 import Cartridge
 import CPU
 import GPU

 import SDL
 
 import Linear (V4(..), V2(..))
 import Linear.Affine

 import Foreign.C.Types

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
   printM msg = GBC $ do
     lift $ putStrLn msg
   pause = GBC $ do
     lift $ getLine >> return ()
   drawPixel pix@(Pixel col@(Color a r g b) x y )  = GBC $ do
     mem <- ask
     rendererDrawColor (renderer mem) $= V4 a r g b
     drawPoint (renderer mem) (P $ V2 (CInt $ fromIntegral x) (CInt $ fromIntegral y))
   showScreen = GBC $ do
     mem <- ask
     present (renderer mem)


 tick :: Emulator m => Cycles -> m ()    
 tick n = do
   load GPU_CYCLES >>= \(MemVal16 cycles) -> store GPU_CYCLES (MemVal16 $ cycles + n)
   GPU.tick n


 defaultWindowConfig :: WindowConfig
 defaultWindowConfig = WindowConfig { windowBorder       = True
                                    , windowHighDPI      = False
                                    , windowInputGrabbed = False
                                    , windowMode         = Windowed
                                    , windowOpenGL       = Nothing
                                    , windowPosition     = Wherever
                                    , windowResizable    = False
                                    , windowInitialSize  = V2 (fromIntegral screen_width)
                                                           (fromIntegral screen_height)
                                    }
     
 runGBC :: GBC a -> IO a
 runGBC (GBC reader) = do
   initializeAll
   window' <- createWindow "HaskellBoy" defaultWindowConfig
   renderer' <- createRenderer window' (-1) defaultRenderer   
   mem <- stToIO $ Memory.new window' renderer'
   runReaderT reader mem

 run :: IO ()
 run = do
   cart <- readCartridge "../roms/blue.gb"
   runGBC $ do
     storeCartridge cart
     forever $ do
       (next, cycles) <- streamNextInstruction
       MemVal16 pc <- load PC
       printM $ (show next) ++ ", " ++ (show cycles) ++ ", " ++ (show pc)
       executeInstruction next
       GBC.tick cycles
       handleInterrupts
       showScreen
