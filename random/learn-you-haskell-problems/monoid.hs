class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

-- Lists are monoids

instance Monoid [a] where
  mempty = []
  mappend = (++)

mappend [3] [1]
-- [3, 1]
mappend mempty [1]
-- [1]
mconcat [[1,2], [3, 4]]
-- [1,2,3,4]

-- exposed in Data.Monoid
newtype Product a =  Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

getProduct $ mappend mempty (Product 8)
-- 8
getProduct $ mappend (Product 5) (Product 8)
-- 40

getSum . mconcat . map Sum $ [1,2,3]
-- 6