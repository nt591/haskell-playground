import System.IO

-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle


main = do
  withFile "girlfriend.txt" ReadMode (\handle -> do
      contents <- hGetContents handle
      putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' filePath mode f = do
  handle <- openFile filePath mode
  results <- f handle
  hClose handle
  return results
