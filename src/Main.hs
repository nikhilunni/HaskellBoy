import GBC

main :: IO ()
main = do
  GBC.run








{-   
   initializeAll
   window <- createWindow "My SDL Application" defaultWindow
   renderer <- createRenderer window (-1) defaultRenderer
   appLoop renderer

 appLoop :: Renderer -> IO ()
 appLoop renderer = do
   events <- pollEvents
   let eventIsQPress event =
         case eventPayload event of
           KeyboardEvent keyboardEvent ->
             keyboardEventKeyMotion keyboardEvent == Pressed &&
             keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
           _ -> False
       qPressed = not (null (filter eventIsQPress events))
   rendererDrawColor renderer $= V4 0 0 255 255
   clear renderer
   present renderer
   unless qPressed (appLoop renderer)

-}
