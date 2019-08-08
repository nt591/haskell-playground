import Data.Char

-- main = do
--   contents <- getContents
--   putStr . shortLinesOnly $ contents


-- This pattern of getting some string from the input, transforming it with a function and
-- then outputting that is so common that there exists a function which makes that even easier,
-- called interact.
-- interact takes a function of type String -> String as a parameter
-- and returns an I/O action that will take some input, run that function
-- on it and then print out the function's result. Let's modify our program to use that.


main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result