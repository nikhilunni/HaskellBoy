import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Word (Word64)
import GBC

main :: IO ()
main = do
  args <- getArgs
  let (ssDir, args') = extractScreenshotDir args
      (cycles, args'') = extractCycles args'
      (flags, rest)  = partition (\a -> a `elem` ["--skip-boot", "--trace", "--jit", "--compare"]) args''
      skipBoot = "--skip-boot" `elem` flags
      trace    = "--trace"     `elem` flags
      jit      = "--jit"       `elem` flags
      compare' = "--compare"   `elem` flags
  case rest of
    [romPath]
      | compare'  -> GBC.runRomJitCompare romPath cycles
      | otherwise -> GBC.run romPath skipBoot trace jit ssDir
    _         -> do
      putStrLn "Usage: haskellboy [--skip-boot] [--trace] [--jit] [--compare] [--cycles N] [--screenshot-dir DIR] <rom-file>"
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

extractCycles :: [String] -> (Word64, [String])
extractCycles [] = (10000000, [])
extractCycles ("--cycles":n:rest) = (read n, rest)
extractCycles (x:xs) = let (c, rest) = extractCycles xs
                       in (c, x:rest)
