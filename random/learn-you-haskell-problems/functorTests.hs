-- IO is a functor
-- IO implements fmap

-- main = do line <- fmap reverse getLine
--           putStrLn $ "you said " ++ line ++ " backwards"


import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn "you said "
          putStrLn line