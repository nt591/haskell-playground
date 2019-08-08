-- http://learnyouahaskell.com/input-and-output

main = do
  line <- getLine
  if null line
    -- return isnt like JS, it takes a value and wraps it in an action
      then return ()
      -- do block because putStrLn and main are both IO actions
      else do
        putStrLn $ reverseWords line
        main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- When dealing with I/O do blocks, we mostly use return either because we need to create an I/O action that doesn't do anything
-- or because we don't want the I/O action that's made up from a do block to have the result value of its last action,
-- but we want it to have a different result value,
-- so we use return to make an I/O action that always has our desired result contained and we put it at the end.

