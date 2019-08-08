import Data.List
import System.IO

data Employee = Employee { name :: String
                         , position :: String
                         , idNum :: Int
                         } deriving (Eq, Show)

samSmith =  Employee { name = "Sam Smith", position = "Mananger", idNum = 1000 }
pamMarx = Employee { name = "Pam Marx", position = "Sales", idNum = 1001 }

isSamPam = samSmith == pamMarx

data ShirtSize = S | M | L
instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"

smallAvail = S `elem` [S, M, L]
showSize = show S

class MyEq a where
  areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False

sayHello = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name


-- TODO - figure out IO
-- writeToFile = do
--   theFile <- openFile "test.txt" WriteMode
--   hPutStrLn theFile ("Random Line of Text")
--   hClose theFile

-- readFromFile = do
--   theFile <- openFile "test.txt" ReadMode
--   contents <- hGetContents theFile
--   putStr contents
--   hClose theFile

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

data Score = Good | Bad
instance Show Score where
  show Good = "it's pretty good"
  show Bad = "give up on your dreams"