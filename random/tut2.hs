import Data.List
import System.IO

addMe :: Int -> Int -> Int
addMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- PATTERN MATCHING
whatAge :: Int -> String
whatAge 16 = "You can drive!"
whatAge 21 = "You can drink!"
whatAge 65 = "You can retire!"
whatAge _ = "Nothing special"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

prodFact :: Int -> Int
prodFact n = product [1..n]


-- GUARDS
isOdd :: Int -> Bool
isOdd n
  | n `mod` 2 == 0 = False
  | otherwise      = True


weightClass :: Float -> Maybe Int
weightClass weight
  | weight <= 59.0  = Just 59
  | weight <= 66.0  = Just 66
  | weight <= 74.0  = Just 74
  | weight <= 83.0  = Just 83
  | weight <= 93.0  = Just 93
  | weight <= 105.0 = Just 105
  | weight <= 120.0 = Just 120
  | otherwise       = Nothing


lifterRanking :: [Int] -> String
lifterRanking wilksLists
  | avg >= 500 = "Worlds"
  | avg >= 450 = "Nationals"
  | avg >= 400 = "Regionals"
  | otherwise  = "Local"
  where
    average lst = div (sum lst) (length lst)
    avg = average . take 3 $ wilksLists


lastElement :: [a] -> Maybe a
lastElement [] = Nothing
lastElement (h : []) = Just h
lastElement (h : t) = lastElement t


areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && (areStringsEq xs ys)
areStringsEq _ _ = False


add2 :: Int -> Int
add2 x = x + 2

doMult :: (Int -> Int) -> Int
doMult fn = fn 3


-- LAMBDA

dbl1to10 = map (\x -> x *2) [1..10]


-- CASE

getClass :: Int -> String

getClass n  =
  case n of
    5 -> "Kindergarten"
    6 -> "Elem"
    _ -> "Everyone else"

