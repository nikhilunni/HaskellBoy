import System.Environment (getArgs)
import System.Exit (exitFailure)
import GBC

main :: IO ()
main = do
  args <- getArgs
  let (ssDir, args') = extractScreenshotDir args
      (flags, rest)  = partition (\a -> a == "--skip-boot" || a == "--trace") args'
      skipBoot = "--skip-boot" `elem` flags
      trace    = "--trace"     `elem` flags
  case rest of
    [romPath] -> GBC.run romPath skipBoot trace ssDir
    _         -> do
      putStrLn "Usage: haskellboy [--skip-boot] [--trace] [--screenshot-dir DIR] <rom-file>"
      exitFailure

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x:xs)
  | p x       = (x : yes, no)
  | otherwise  = (yes, x : no)
  where (yes, no) = partition p xs

extractScreenshotDir :: [String] -> (Maybe FilePath, [String])
extractScreenshotDir [] = (Nothing, [])
extractScreenshotDir ("--screenshot-dir":dir:rest) = (Just dir, rest)
extractScreenshotDir (x:xs) = let (d, rest) = extractScreenshotDir xs
                              in (d, x:rest)
