{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Monad
        ( emulationError ) where

 import System.Exit
 import System.IO

 import Control.Monad.IO.Class (liftIO)
 import Effectful (Eff, IOE, (:>))

 emulationError :: IOE :> es => String -> Eff es ()
 emulationError msg = liftIO $ hPutStrLn stderr msg >> exitFailure
