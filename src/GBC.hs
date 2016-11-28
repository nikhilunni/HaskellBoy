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

 import Control.Lens

 import Data.Word (Word8, Word16)
 import Data.STRef

 import Types
 import Memory
 import Monad
 import Cartridge
 import CPU
 import GPU
-- import MemoryRules

 import SDL

 import Linear (V4(..), V2(..))
 import Linear.Affine

 import Foreign.C.Types

 import System.Exit
 import System.IO

 tick :: Cycles -> GBC ()
 tick n = do
   readRef gpu_cycles >>= \cycles -> writeRef gpu_cycles $ cycles + n
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
   mem <- stToIO $ Memory.new window' renderer' True 0
   runReaderT reader mem

 run :: IO ()
 run = do
   cart <- readCartridge "../roms/blue.gb"
   runGBC $ do
     storeCartridge Memory.boot_rom
     forever $ do
       pcVal <- readRef pc
       (next, cycles) <- streamNextInstruction
       printM $ (show next) ++ ", " ++ (show cycles) ++ ", " ++ (show pcVal)
       executeInstruction next
       GBC.tick cycles
       handleInterrupts
       showScreen
