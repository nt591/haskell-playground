-- The newtype keyword in Haskell is made exactly for these cases when we want to just take one type
-- and wrap it in something to present it as another type.

newtype ZipList a = ZipList { getZipList :: [a] }


-- Instead of the data keyword, the newtype keyword is used.
-- Now why is that? Well for one, newtype is faster.
-- If you use the data keyword to wrap a type, there's some overhead to all that wrapping and unwrapping when your program is running.
-- But if you use newtype, Haskell knows that you're just using it to wrap an existing type into a new type (hence the name),
-- because you want it to be the same internally but have a different type.
-- With that in mind, Haskell can get rid of the wrapping and unwrapping once it resolves which value is of what type.

-- So why not just use newtype all the time instead of data then?
-- Well, when you make a new type from an existing type by using the newtype keyword,
-- you can only have one value constructor and that value constructor can only have one field.
-- But with data, you can make data types that have several value constructors and each constructor can have zero or more fields:

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
x = CharList "Hello"
getCharList x
-- "Hello"
