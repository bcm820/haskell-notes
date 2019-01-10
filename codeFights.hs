import           Data.List

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

{-
Given a sequence of integers as an array,
determine whether it is possible to obtain a strictly increasing
sequence by removing no more than one element from the array.
For sequence = [1, 3, 2, 1], the output should be false.
For sequence = [1, 3, 2], the output should be true.
You can remove 3 from the array to get the strictly increasing sequence [1, 2].
Alternately, you can remove 2 to get the strictly increasing sequence [1, 3].
-}

almostIncreasingSequence :: [Int] -> Bool
almostIncreasingSequence [] = True
almostIncreasingSequence [_] = True
almostIncreasingSequence [_,_] = True
almostIncreasingSequence (a:b:c:ds)
    | a < b && b < c = almostIncreasingSequence (b:c:ds)
    | a < b && a < c = isIncreasing (a:c:ds)
    | a > b && b < c = isIncreasing (b:c:ds)
    | a < b && c <= a = isIncreasing (a:b:ds)
    | otherwise = False

isIncreasing :: [Int] -> Bool
isIncreasing [_] = True
isIncreasing (a:b:cs)
    | a < b = isIncreasing (b:cs)
    | otherwise = False

{-
The CodeBots all decided to move to a new building and live together.
The building is represented by a rectangular matrix of rooms.
Each cell in the matrix contains an integer, the price of the room.
Some rooms are free but haunted, so all the bots are afraid of them.
That is why any room that is free or located in the same column
is not considered suitable for the bots to live in.
Help calculate the total price of all suitable rooms in the building.
For matrix = [[0, 1, 1, 2],
              [0, 5, 0, 0],
              [2, 0, 3, 3]], the total is 9.
-}

matrixElementsSum :: [[Int]] -> Int
matrixElementsSum matrix =
    let sumRow row = sum $ takeWhile (>0) row
    in  sum $ map sumRow $ transpose matrix

{-
Given an array of strings, return another array
containing all of its longest strings.
For inputArray = ["aba", "aa", "ad", "vcd", "aba"],
the output should be ["aba", "vcd", "aba"].
-}

allLongestStrings :: [[Char]] -> [[Char]]
allLongestStrings xxs =
    let maxLength = maximum $ map length xxs
        filterMax xs = length xs == maxLength
    in  filter filterMax xxs

{-
Given an array of strings, return another array
containing all of its longest strings.
For inputArray = ["aba", "aa", "ad", "vcd", "aba"],
the output should be ["aba", "vcd", "aba"].
-}

allLongestStrings :: [[Char]] -> [[Char]]
allLongestStrings xxs =
    let maxLength = maximum $ map length xxs
        filterMax xs = length xs == maxLength
    in  filter filterMax xxs

{-
Given two strings, find the number of common characters between them.
For s1 = "aabcc" and s2 = "adcaa", the output should be 3.
The strings have 3 common characters - 2 "a"s and 1 "c".
-}

commonCharacterCount :: [Char] -> [Char] -> Int
commonCharacterCount s1 s2 =
  length $ snd (foldl consume (s2, "") s1)

consume :: ([Char], [Char]) -> Char -> ([Char], [Char])
consume ss@(s2, s3) c
    | elem c s2 = (delete c s2, s3 ++ [c])
    | otherwise = ss

{-
Ticket numbers usually consist of an even number of digits.
A ticket number is considered lucky if the sum of the first
half of the digits is equal to the sum of the second half.
Given a ticket number n, determine if it's lucky or not.
For n = 1230, the output should be true;
For n = 239017, the output should be false.
-}

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

sumIntChars :: [Char] -> Int
sumIntChars cs = sum $ map (\c -> read [c]) cs

isLucky :: Int -> Bool
isLucky n =
    let (h1, h2)        = halve (show n)
    in  sumIntChars h1 == sumIntChars h2

{-
Some people are standing in a row in a park.
There are trees between them which cannot be moved.
Your task is to rearrange the people by their heights
in a non-descending order without moving the trees.
For a = [-1, 150, 190, 170, -1, -1, 160, 180], the output
should be [-1, 150, 160, 170, -1, -1, 180, 190].
-}

reinsert :: ([Int], [Int]) -> Int -> ([Int], [Int])
reinsert (ps,ys) x
    | x == -1   = (ps,(x:ys))
    | otherwise = ((tail ps),((head ps):ys))

sortByHeight :: [Int] -> [Int]
sortByHeight xs =
    let ps = sort (filter (/=(-1)) xs)
    in  reverse $ snd (foldl reinsert (ps,[]) xs)

-- A similar solution by Ciunkos:
zipC :: [Int] -> [Int] -> [Int]
zipC a      _      = a
zipC (a:as) (b:bs)
    | a >= 0    = b : zipC as bs
    | otherwise = a : zipC as (b:bs)
sortByHeight a = zipC a (sort (filter (>= 0) a))

