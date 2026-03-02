{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Effects.Handlers.SDL
  ( runDisplaySDL
  , runAudioOutSDL
  , runInputSDL
  , SDLDisplay(..)
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

import Control.Monad (when, forM_, void)
import Control.Monad.ST (stToIO)
import Data.Word
import Data.Bits
import Data.Array.ST (writeArray, getElems)
import Data.IORef
import qualified Data.ByteString as BS

import Foreign.C.Types
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr)

import qualified SDL
import qualified SDL.Raw.Audio as RawAudio

import System.IO (hPutStrLn, stderr)

import Effects.Display (Display(..), captureFrame)
import Effects.AudioOut (AudioOut(..))
import Effects.Input (Input(..))
import Effects.Env (EmulatorEnv, withMemory)

import Types
import Memory (readRef, writeRef, readArrI16, requestInterrupt)

data SDLDisplay = SDLDisplay
  { sdlWindow       :: SDL.Window
  , sdlRenderer     :: SDL.Renderer
  , sdlLastFrameRef :: IORef Word32
  , sdlScreenshotDir :: IORef (Maybe FilePath)
  }

runDisplaySDL :: (EmulatorEnv :> es, IOE :> es)
              => SDLDisplay -> Eff (Display : es) a -> Eff es a
runDisplaySDL st = interpret $ \_ -> \case
  PutPixel !x !y !a !r !g !b -> do
    liftIO $ do
      SDL.rendererDrawColor (sdlRenderer st) SDL.$= SDL.V4 r g b a
      SDL.drawPoint (sdlRenderer st)
        (SDL.P $ SDL.V2 (CInt $ fromIntegral x) (CInt $ fromIntegral y))
    when (x >= 0 && x < 160 && y >= 0 && y < 144) $
      withMemory $ \mem -> stToIO $ do
        let !idx = (y * 160 + x) * 3
        writeArray (framebuffer mem) idx b
        writeArray (framebuffer mem) (idx + 1) g
        writeArray (framebuffer mem) (idx + 2) r

  PresentFrame -> do
    liftIO $ SDL.present (sdlRenderer st)
    -- Frame rate limiting
    now <- liftIO SDL.ticks
    lastT <- liftIO $ readIORef (sdlLastFrameRef st)
    let elapsed = now - lastT
    when (elapsed < 17) $ liftIO $ SDL.delay (17 - elapsed)
    liftIO $ SDL.ticks >>= writeIORef (sdlLastFrameRef st)

  ClearFrame -> liftIO $ do
    SDL.rendererDrawColor (sdlRenderer st) SDL.$= SDL.V4 255 255 255 255
    SDL.clear (sdlRenderer st)

  CaptureFrame path -> do
    pixels <- withMemory $ \mem -> stToIO $ getElems (framebuffer mem)
    liftIO $ do
      let pixelData = BS.pack pixels
          width  = 160 :: Int
          height = 144 :: Int
          rowSize = width * 3
          imageSize = rowSize * height
          fileSize = 14 + 40 + imageSize
          fileHeader = BS.pack $
            [0x42, 0x4D]
            ++ word32LE fileSize
            ++ [0,0,0,0]
            ++ word32LE 54
          dibHeader = BS.pack $
            word32LE 40
            ++ int32LE width
            ++ int32LE height
            ++ word16LE 1
            ++ word16LE 24
            ++ word32LE 0
            ++ word32LE imageSize
            ++ word32LE 2835
            ++ word32LE 2835
            ++ word32LE 0
            ++ word32LE 0
          flippedPixels = BS.concat
            [ BS.take rowSize (BS.drop (row * rowSize) pixelData)
            | row <- [height - 1, height - 2 .. 0]
            ]
          bmpData = BS.concat [fileHeader, dibHeader, flippedPixels]
      BS.writeFile path bmpData

runAudioOutSDL :: (EmulatorEnv :> es, IOE :> es)
               => Word32 -> Eff (AudioOut : es) a -> Eff es a
runAudioOutSDL devID = interpret $ \_ -> \case
  FlushAudio -> do
    bufPos <- readRef audioBufPos
    when (bufPos > 0) $ do
      when (devID /= 0) $ do
        samples <- mapM (readArrI16 audioBuffer) [0..bufPos-1]
        liftIO $ withArray samples $ \ptr ->
          void $ RawAudio.queueAudio devID (castPtr ptr :: Ptr ()) (fromIntegral $ bufPos * 2)
      writeRef audioBufPos 0

runInputSDL :: (EmulatorEnv :> es, Display :> es, IOE :> es)
            => IORef (Maybe FilePath) -> Eff (Input : es) a -> Eff es a
runInputSDL ssDirRef = interpret $ \_ -> \case
  PollInput -> do
    events <- liftIO SDL.pollEvents
    let isQuitEvt = any (\e -> SDL.eventPayload e == SDL.QuitEvent) events
    when isQuitEvt $ liftIO $ fail "quit"
    -- F12 screenshot
    let isF12 = any isF12Press events
    when isF12 $ do
      dir <- liftIO $ readIORef ssDirRef
      let path = case dir of
                   Just d  -> d ++ "/screenshot.bmp"
                   Nothing -> "screenshot.bmp"
      Effects.Display.captureFrame path
      liftIO $ hPutStrLn stderr $ "Screenshot saved to " ++ path
    -- Continuous screenshot if dir is set
    dir <- liftIO $ readIORef ssDirRef
    case dir of
      Just d  -> Effects.Display.captureFrame (d ++ "/screenshot.bmp")
      Nothing -> pure ()
    -- Process joypad input
    forM_ events $ \event -> case SDL.eventPayload event of
      SDL.KeyboardEvent kd -> do
        let motion = SDL.keyboardEventKeyMotion kd
            key = SDL.keysymKeycode (SDL.keyboardEventKeysym kd)
        case keyToButton key of
          Just (isDir, bitIdx) -> do
            let ref = if isDir then joypadDirPad else joypadButtons
            val <- readRef ref
            case motion of
              SDL.Pressed -> do
                when (testBit val bitIdx) $ requestInterrupt Joypad
                writeRef ref (clearBit val bitIdx)
              SDL.Released -> writeRef ref (setBit val bitIdx)
          Nothing -> return ()
      _ -> return ()

  CheckQuit -> return False  -- quit is handled via exception above

  where
    isF12Press e = case SDL.eventPayload e of
      SDL.KeyboardEvent kd ->
        SDL.keyboardEventKeyMotion kd == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym kd) == SDL.KeycodeF12
      _ -> False

keyToButton :: SDL.Keycode -> Maybe (Bool, Int)
keyToButton SDL.KeycodeZ         = Just (False, 0)
keyToButton SDL.KeycodeX         = Just (False, 1)
keyToButton SDL.KeycodeBackspace = Just (False, 2)
keyToButton SDL.KeycodeReturn    = Just (False, 3)
keyToButton SDL.KeycodeRight     = Just (True,  0)
keyToButton SDL.KeycodeLeft      = Just (True,  1)
keyToButton SDL.KeycodeUp        = Just (True,  2)
keyToButton SDL.KeycodeDown      = Just (True,  3)
keyToButton _                    = Nothing

-- BMP encoding helpers
word32LE :: Int -> [Word8]
word32LE n = [ fromIntegral (n .&. 0xFF)
             , fromIntegral ((n `shiftR` 8) .&. 0xFF)
             , fromIntegral ((n `shiftR` 16) .&. 0xFF)
             , fromIntegral ((n `shiftR` 24) .&. 0xFF)
             ]

int32LE :: Int -> [Word8]
int32LE = word32LE

word16LE :: Int -> [Word8]
word16LE n = [ fromIntegral (n .&. 0xFF)
             , fromIntegral ((n `shiftR` 8) .&. 0xFF)
             ]
