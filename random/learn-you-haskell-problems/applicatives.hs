x = fmap (*) [1,2,3]
-- fmap (*) [1,2,3] :: Num a => [a -> a]

-- We see how by mapping "multi-parameter" functions over functors, we get functors that contain functions inside them.
-- So now what can we do with them?
-- Well for one, we can map functions that take these functions as parameters over them,
-- because whatever is inside a functor will be given to the function that we're mapping over it as a parameter.

mapped = fmap (\fn -> fn 5) x
-- [5,10,15]

-- basically partially applying functions to values inside functors to get functors of partially applied functions
-- Those can then be fmap'd with the rest of the args (fmapping functions that take those functions) to get a functor of values

-- Definition of Applicative
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Some F must be a functor before it can be an applicative
-- pure takes some a and returns an applicative context of a
-- <*> takes an applicative holding an a->b function and an applicative of a and returns an applicative of b

fmap (*) [1,2,3] <*> [5]
-- [5,10,15]

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something

c = pure (*5)
c <*> Just 5
-- Just 25
c <*> Nothing
-- Nothing

pure (+) <*> Just 3 <*> Just 5
-- Just 8

add3 x y z = x + y + z
pure add3 <*> [1] <*> [2] <*> [3]
-- [6]
pure add3 <*> Just 5 <*> Just 3 <*> Just 1
-- Just 9

(<$>) :: Functor f => (a -> b) -> f a -> f b
-- <$> is just an infix fmap for applicative functors

(*) <$> Just 5 <*> Just 5
-- Just 25

-- Awesome! To use a normal function on applicative functors, just sprinkle some <$> and <*> about
--  and the function will operate on applicatives and return an applicative. How cool is that?

-- Lists are also applicatives
instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]


(*) <$> [1,2,3] <*> [2]
-- [2,4,6]
(*) <$> [1,2,3] <*> [2,4]
-- [2,4,4,8,6,12]
(*) <$> [1,2,3] <*> [2,4,5]
-- [2,4,5,4,8,10,6,12,15]

[(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]


instance Applicative IO where
  pure = return
  a <*> b = do
      f <- a
      x <- b
      return (f x)

-- which means

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

-- becomes

myAction2 :: IO String
myAction2 = (++) <$> getLine <*> getLine


instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)

(+) <$> (+3) <*> (*100) $ 5
-- 508
-- Applies 5 to *100, applies 5 to +3, calls + with those


-- You can think of functions as boxes that contain their eventual results, so doing k <$> f <*> g
-- creates a function that will call k with the eventual results from f and g.
-- When we do something like (+) <$> Just 3 <*> Just 5, we're using + on values that might or might not be there,
--  which also results in a value that might or might not be there.
--  When we do (+) <$> (+10) <*> (+5), we're using + on the future return values of (+10) and (+5)
--  and the result is also something that will produce a value only when called with a parameter.

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [5,6,7,8]
-- [6,8,10]

-- from Control.Applicative
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

liftA2 (+) (Just 5) (Just 6)
-- Just 11

liftA2 (:) (Just 3) (Just [4])
-- Just [3,4]



-- Let's try implementing a function that takes a list of applicatives and returns an
-- applicative that has a list as its result value. We'll call it sequenceA.
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs