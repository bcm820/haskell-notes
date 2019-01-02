{-
Given a year, return the century it is in.
The first century spans from the year 1 up to
and including the year 100, the second - from
the year 101 up to and including the year 200, etc.
For y = 1905, the output should be 20;
For y = 1700, the output should be 17.
-}
centuryFromYear :: Int -> Int
centuryFromYear y
  | m == 0    = d
  | otherwise = d + 1
  where
    m = mod y 100
    d = div y 100

{-
Given the string, check if it is a palindrome.
For str = "aabaa", the output should be true.
For str = "abac", the output should be false.
For str = "a", the output should be true.
-}
checkPalindrome :: [Char] -> Bool
checkPalindrome str = str == reverse str

{-
Given an array of integers, find the pair of
adjacent elements that has the largest product
and return that product.
For xs = [3, 6, -2, -5, 7, 3], the output
should be 21 (produced from 7 and 3).
-}
adjacentElementsProduct :: [Int] -> Int
adjacentElementsProduct xs =
    let tupleAcc acc x = (snd (head acc)*x, x) : acc
        initVal = [(0, head xs)]
    in  maximum (init (map fst (foldl tupleAcc initVal (tail xs))))

-- Other solutions by nickie, ameiva, code_g5:
adjacentElementsProduct xs = maximum $ zipWith (*) x (tail xs)
adjacentElementsProduct xs = foldl1 max $ zipWith (*) xs (tail xs)
adjacentElementsProduct = maximum . products
    where products []       = []
          products (x:[])   = []
          products (x:y:xs) = x * y : products (y:xs)

{-
Ratiorg got statues of different sizes as a present from CodeMaster,
each statue having an non-negative integer size.
Since he likes to make things perfect, he wants to arrange them
from smallest to largest so that each statue will be bigger than
the previous exactly by 1. He may need some additional statues to be
able to accomplish that. Help him figure out the minimum number of
additional statues needed.
For statues = [6, 2, 3, 8], the output should be 3.
Ratiorg needs statues of sizes 4, 5 and 7.
-}
makeArrayConsecutive2 :: [Int] -> Int
makeArrayConsecutive2 xs = length ([(minimum xs)..(maximum xs)]) - length xs
