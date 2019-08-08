import Data.List
import System.IO

-- primeNumbers = [3,5,7,11]
-- primeNumbers = 3 : 5 : 7 : 11 : []

-- morePrime = primeNumbers ++ [13, 17]

-- moreMorePrime = 1 : morePrime

-- zeroToTen = [0..10]

-- doubles = [ x * 2 | x <- zeroToTen ]

-- takeFirstTenEvens = [ x | x <- take 10 . filter even  $ [0..]]

-- takeAllTriplesUnder50 = [x * 3 | x <- [1..], x * 3 < 50]

-- doubleFirst10NumbersDivisibleBy10And14 = take 10 [ x * 2 |  x <- [1..], mod x 10 == 0, mod x 14 == 0]

pow3List = [ 3^n | n <- [1..10]]
multTable = [[x * y | x <- [1..10]] | y <- [1..10]]

bobSmith = ("Bob Smith", 35)
name = fst bobSmith
age = snd bobSmith

names = "Bob" : "Mary" : "Alice" : []
cities = "New York" : "San Francisco" : "Boston" : []
peoples = zip names cities