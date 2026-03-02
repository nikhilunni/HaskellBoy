{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Effects.Trace
  ( Trace(..)
  , traceInstr
  , traceMsg
  , runTraceSilent
  , runTraceStderr
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

import Data.Word
import Numeric (showHex)
import System.IO (hPutStrLn, stderr)

data Trace :: Effect where
  TraceInstr :: Word16 -> String -> Trace m ()
  TraceMsg   :: String -> Trace m ()

type instance DispatchOf Trace = 'Dynamic

traceInstr :: Trace :> es => Word16 -> String -> Eff es ()
traceInstr pcVal instr = send (TraceInstr pcVal instr)

traceMsg :: Trace :> es => String -> Eff es ()
traceMsg msg = send (TraceMsg msg)

runTraceSilent :: Eff (Trace : es) a -> Eff es a
runTraceSilent = interpret $ \_ -> \case
  TraceInstr _ _ -> pure ()
  TraceMsg _ -> pure ()

runTraceStderr :: IOE :> es => Eff (Trace : es) a -> Eff es a
runTraceStderr = interpret $ \_ -> \case
  TraceInstr pcVal instr -> liftIO $ hPutStrLn stderr $ "PC=" ++ showHex16 pcVal ++ " " ++ instr
  TraceMsg msg -> liftIO $ hPutStrLn stderr msg

showHex16 :: Word16 -> String
showHex16 w = let s = showHex w ""
              in replicate (4 - length s) '0' ++ s
