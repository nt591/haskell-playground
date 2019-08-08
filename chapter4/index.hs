module Exercises where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- Write a function that tells you whether or not a given
-- String (or list) is a palindrome. Here you’ll want to use a
-- function called reverse a predefined function that does
-- what it sounds like

palindrome :: String -> Bool
palindrome x = x == reverse x


-- Write a function to return the absolute value of a number
-- using if-then-else
myAbs :: Integer -> Integer
myAbs num = if num < 0 then negate num else num

-- Fill in the definition of the following function, using fst
-- and snd:

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f tuple1 tuple2 = ((snd tuple1, snd tuple2), (fst tuple1, fst tuple2))