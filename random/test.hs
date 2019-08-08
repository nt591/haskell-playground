module SampleFunc (getClass, doubleEvenNumbers) where

  getClass n = n
  doubleEvenNumbers n = n

  data BaseballPlayer
    = Pitcher
    | Catcher
    | Infielder
    | Outfield
    deriving Show

  data Customer = Customer String String Double deriving Show

  tomSmith :: Customer
  tomSmith = Customer "Tom" "NY" 20.50

  getBalance :: Customer -> Double

  getBalance (Customer _ _ b) = b


  data RPS
    = Rock
    | Paper
    | Scissors

  shoot :: RPS -> RPS -> String
  shoot Paper Rock = "Paper beats Rock"
  shoot Rock Scissors = "Rock beats Scissors"
  shoot Scissors Paper = "Scissors beats Paper"
  shoot Scissors Rock = "Scissors loses to Rock"
  shoot Paper Scissors = "Paper loses to Scissors"
  shoot Rock Paper = "Rock loses to Paper"


  data Shape
    = Circle Float Float Float
    | Rectangle Float Float Float Float
    deriving Show

  area :: Shape -> Float
  area (Circle _ _ r) = r ^ 2 * pi
  area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)

  sumValue = putStrLn . show $ 1+ 2
  areaRect = area $ Rectangle 10 10 100 100
  areaCirc = area $ Circle 10 10 100