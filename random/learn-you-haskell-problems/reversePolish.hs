import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingExpression [] . words
  where foldingExpression (x:y:ys) "*" = (x * y):ys
        foldingExpression (x:y:ys) "+" = (x + y):ys
        foldingExpression (x:y:ys) "-" = (y - x):ys
        foldingExpression (x:y:ys) "/" = (y / x):ys
        foldingExpression (x:y:ys) "^" = (y ** x):ys
        foldingExpression (x:xs) "ln"  = log x:xs
        foldingExpression xs "sum"     = [sum xs]
        foldingExpression xs numberString = read numberString:xs