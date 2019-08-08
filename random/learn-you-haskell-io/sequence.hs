-- http://learnyouahaskell.com/input-and-output

main = do
  mapM_ print [1,2,3]
  rs <- sequence [getLine, getLine, getLine]
  print rs


  -- A common pattern with sequence is when we map functions like print or putStrLn over lists.
  -- Doing map print [1,2,3,4] won't create an I/O action.
  -- It will create a list of I/O actions, because that's like writing
  -- [print 1, print 2, print 3, print 4].
  -- If we want to transform that list of I/O actions into an I/O action, we have to sequence it.


-- Because mapping a function that returns an I/O action over a list and then sequencing it is so common,
-- the utility functions mapM and mapM_ were introduced.
-- mapM takes a function and a list, maps the function over the list and then sequences it.
-- mapM_ does the same, only it throws away the result later.
-- We usually use mapM_ when we don't care what result our sequenced I/O actions have.

