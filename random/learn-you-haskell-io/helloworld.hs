import Data.Char
-- Do notation glues multiple IO actions into one IO action

main = do
  putStrLn "What's your first name?"
  -- perform the getLine action and bind its result to name
  firstName <- getLine
  -- return value of a do block is the last value returned, cannot be bound

  putStrLn "What's your last name?"
  lastName <- getLine

  let upperFirstName = map toUpper firstName
      upperLastName = map toUpper lastName
  putStrLn $ "Hello " ++ upperFirstName ++ " " ++ upperLastName ++ "!"