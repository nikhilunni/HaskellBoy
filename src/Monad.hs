module Monad
        where

 import Control.Monad (when)
 import Control.Monad.Reader
 import Control.Monad.ST (stToIO)

 import SDL

 import Foreign.C.Types

 import Data.Word
 import Data.Bits
 import Data.Array.ST (writeArray, getElems)
 import qualified Data.ByteString as BS

 import System.Exit
 import System.IO

 import Types
 import Memory

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
 drawPixel (Pixel (Color a r g b) x y) = do
   hl <- readRef headless
   when (not hl) $ GBC $ do
     mem <- ask
     rendererDrawColor (renderer mem) $= V4 r g b a
     drawPoint (renderer mem) (P $ V2 (CInt $ fromIntegral x) (CInt $ fromIntegral y))
     -- Write BGR to framebuffer for BMP compatibility (guard out-of-bounds from scrolling)
     when (x >= 0 && x < 160 && y >= 0 && y < 144) $ do
       let idx = (fromIntegral y * 160 + fromIntegral x) * 3
       lift $ stToIO $ do
         writeArray (framebuffer mem) idx b
         writeArray (framebuffer mem) (idx + 1) g
         writeArray (framebuffer mem) (idx + 2) r

 showScreen :: GBC ()
 showScreen = do
   hl <- readRef headless
   when (not hl) $ GBC $ do
     mem <- ask
     present (renderer mem)

 captureScreenshot :: FilePath -> GBC ()
 captureScreenshot path = do
   hl <- readRef headless
   when (not hl) $ GBC $ do
     mem <- ask
     pixels <- lift $ stToIO $ getElems (framebuffer mem)
     let pixelData = BS.pack pixels
         width  = 160 :: Int
         height = 144 :: Int
         rowSize = width * 3  -- 480, already 4-byte aligned
         imageSize = rowSize * height
         fileSize = 14 + 40 + imageSize
         -- BMP file header (14 bytes)
         fileHeader = BS.pack $
           [0x42, 0x4D]                        -- "BM"
           ++ word32LE fileSize                 -- file size
           ++ [0,0,0,0]                         -- reserved
           ++ word32LE 54                       -- pixel data offset
         -- DIB header (BITMAPINFOHEADER, 40 bytes)
         dibHeader = BS.pack $
           word32LE 40                          -- header size
           ++ int32LE width                     -- width
           ++ int32LE height                    -- height (positive = bottom-up)
           ++ word16LE 1                        -- color planes
           ++ word16LE 24                       -- bits per pixel
           ++ word32LE 0                        -- compression (BI_RGB)
           ++ word32LE imageSize                -- image size
           ++ word32LE 2835                     -- horizontal resolution (72 DPI)
           ++ word32LE 2835                     -- vertical resolution (72 DPI)
           ++ word32LE 0                        -- colors in palette
           ++ word32LE 0                        -- important colors
         -- Flip rows: BMP stores bottom-up, framebuffer is top-down
         flippedPixels = BS.concat
           [ BS.take rowSize (BS.drop (row * rowSize) pixelData)
           | row <- [height - 1, height - 2 .. 0]
           ]
         bmpData = BS.concat [fileHeader, dibHeader, flippedPixels]
     lift $ BS.writeFile path bmpData

 pollSDLEvents :: GBC ()
 pollSDLEvents = do
   hl <- readRef headless
   when (not hl) $ do
     events <- liftIO pollEvents
     let isQuit = any (\e -> eventPayload e == QuitEvent) events
         isF12  = any isF12Press events
     when isQuit $ liftIO exitSuccess
     when isF12 $ do
       dir <- readRef screenshotDir
       let path = case dir of
                    Just d  -> d ++ "/screenshot.bmp"
                    Nothing -> "screenshot.bmp"
       captureScreenshot path
       liftIO $ hPutStrLn stderr $ "Screenshot saved to " ++ path
   where
     isF12Press e = case eventPayload e of
       KeyboardEvent kd ->
         keyboardEventKeyMotion kd == Pressed &&
         keysymKeycode (keyboardEventKeysym kd) == KeycodeF12
       _ -> False

 -- Helpers for little-endian BMP encoding
 word32LE :: Int -> [Word8]
 word32LE n = [ fromIntegral (n .&. 0xFF)
              , fromIntegral ((n `shiftR` 8) .&. 0xFF)
              , fromIntegral ((n `shiftR` 16) .&. 0xFF)
              , fromIntegral ((n `shiftR` 24) .&. 0xFF)
              ]

 int32LE :: Int -> [Word8]
 int32LE = word32LE  -- same encoding, just conceptually signed

 word16LE :: Int -> [Word8]
 word16LE n = [ fromIntegral (n .&. 0xFF)
              , fromIntegral ((n `shiftR` 8) .&. 0xFF)
              ]
