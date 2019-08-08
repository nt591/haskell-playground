-- http://learnyouahaskell.com/input-and-output

respondPalindrome = unlines . map (\str -> if isPalindrome str then "palindrome" else "not palindrome") . lines
  where isPalindrome xs = xs == reverse xs

main = interact respondPalindrome