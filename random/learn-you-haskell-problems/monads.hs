-- If we have a fancy value and a function that takes a normal value but returns a fancy value, how do we feed that fancy value into the function?
-- This is the main question that we will concern ourselves when dealing with monads.
-- We write m a instead of f a because the m stands for Monad, but monads are just applicative functors that support >>=.
-- The >>= function is pronounced as bind.

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- exampleFn
-- (\x -> Just (x + 1))
-- function that takes some type a, and returns a monadic b
-- how would we apply this to a function of Maybe a?

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) fn = fn x

-- applyMaybe (Just 5) (\x -> Just (x + 2))
-- Just 7

-- applyMaybe Nothing (\x -> Just (x + 2))
-- Nothing

class Monad m where
  -- remember return in IO? Lifts a into context
  -- same as applicative Pure
  return :: a -> m a

  -- bind
  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  fail :: String -> m a
  fail msg = error msg


-- Maybe is a monad!

instance Monad Maybe where
  return x      = Just x
  Nothing >>= _ = Nothing
  Just x >>= f  = f x
  fail _        = Nothing


-- http://learnyouahaskell.com/a-fistful-of-monads

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

-- helper fn to chain functions right to left
x -: f = f x

-- (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2

-- update to handle chaining of checking state with maybes!
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left,right)| abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- now try with monadic composition
return (0,0) >>= landLeft' 1 >>= landRight' 2  >>= landLeft' 10 >>= landRight' 2
-- Nothing! Failed at landLeft' 10

return (0,0) >>= landLeft' 1 >>= landRight' 2  >>= landLeft' 1 >>= landRight' 2
-- Just (2, 4)



-- DO BLOCKS

foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

-- If any of the values that we try to extract from are Nothing, the whole do expression will result in a Nothing.

-- Do blocks are just another syntax for chaining monadic values

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft' 2 start
  second <- landRight' 2 first
  landLeft' 1 second

-- Without monads
routine' :: Maybe Pole
routine' =
    case Just (0,0) of
        Nothing -> Nothing
        Just start -> case landLeft 2 start of
            Nothing -> Nothing
            Just first -> case landRight 2 first of
                Nothing -> Nothing
                Just second -> landLeft 1 second