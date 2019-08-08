-- Functor Laws
-- First law: identiy -> fmap id F == F
-- Second law: composition -> fmap f (fmap g F) == fmap (f . g) F


data CMaybe a = CNothing | CJust Int a deriving (Show, Eq)
-- C is for Counter

-- Turn into (invalid) instance of Functor

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter val) = CJust (counter+1) (f val)

(CJust 0 "haha")  == fmap id (CJust 0 "haha") -- False

-- CMaybe is an invalid functor because it does not obey the identity law
