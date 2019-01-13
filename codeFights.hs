import           Data.Char
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
centuryFromYear y = if m == 0 then d else d + 1
  where m = mod y 100; d = div y 100

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
  maximum . init . map fst $ foldl tupleAcc initVal $ tail xs where
    tupleAcc acc x = (x * snd (head acc), x) : acc
    initVal = [(0, head xs)]

-- Other solutions by nickie, ameiva, code_g5:
adjacentElementsProduct' xs = maximum $ zipWith (*) xs (tail xs)
adjacentElementsProduct'' xs = foldl1 max $ zipWith (*) xs (tail xs)
adjacentElementsProduct''' = maximum . products where
  products []       = []
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
makeArrayConsecutive2 xs = length minToMax - length xs
  where minToMax = [(minimum xs)..(maximum xs)]

{-
Given a sequence of integers as an array,
determine whether it is possible to obtain a strictly increasing
sequence by removing no more than one element from the array.
For sequence = [1, 3, 2, 1], the output should be false.
For sequence = [1, 3, 2], the output should be true.
You can remove 3 from the array to get the strictly increasing sequence [1, 2].
Alternately, you can remove 2 to get the strictly increasing sequence [1, 3].
-}

isIncreasing :: [Int] -> Bool
isIncreasing [_] = True
isIncreasing (a:b:cs)
  | a < b = isIncreasing (b:cs)
  | otherwise = False

almostIncreasingSequence :: [Int] -> Bool
almostIncreasingSequence [] = True
almostIncreasingSequence [_] = True
almostIncreasingSequence [_,_] = True
almostIncreasingSequence (a:b:c:ds)
  | a < b && b < c  = almostIncreasingSequence (b:c:ds)
  | a < b && a < c  = isIncreasing (a:c:ds)
  | a > b && b < c  = isIncreasing (b:c:ds)
  | a < b && c <= a = isIncreasing (a:b:ds)
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
matrixElementsSum = sum . map sumRow . transpose
  where sumRow = sum . takeWhile (>0)

{-
Given an array of strings, return another array
containing all of its longest strings.
For inputArray = ["aba", "aa", "ad", "vcd", "aba"],
the output should be ["aba", "vcd", "aba"].
-}

allLongestStrings :: [[Char]] -> [[Char]]
allLongestStrings xxs = filter filterMax xxs where
  filterMax xs = length xs == maxLength
  maxLength = maximum $ map length xxs

{-
Given two strings, find the number of common characters between them.
For s1 = "aabcc" and s2 = "adcaa", the output should be 3.
The strings have 3 common characters - 2 "a"s and 1 "c".
-}

commonCharacterCount :: [Char] -> [Char] -> Int
commonCharacterCount s1 s2 = length $ snd (foldl consume (s2, "") s1) where
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

isLucky :: Int -> Bool
isLucky n = sumIntChars h1 == sumIntChars h2 where
  sumIntChars cs = sum $ map (\c -> read [c]) cs
  (h1, h2) = splitInHalf (show n)
  splitInHalf xs = splitAt (length xs `div` 2) xs

{-
Some people are standing in a row in a park.
There are trees between them which cannot be moved.
Your task is to rearrange the people by their heights
in a non-descending order without moving the trees.
For a = [-1, 150, 190, 170, -1, -1, 160, 180], the output
should be [-1, 150, 160, 170, -1, -1, 180, 190].
-}

reinsert :: ([Int], [Int]) -> Int -> ([Int], [Int])
reinsert ([], ys) x     = ([], x:ys)
reinsert (ps,ys) (-1)   = (ps, (-1:ys))
reinsert ((p:qs), ys) x = (qs, (p:ys))

sortByHeight :: [Int] -> [Int]
sortByHeight xs = reverse $ snd $ foldl reinsert (ps,[]) xs
  where ps = sort . filter (/=(-1)) $ xs

-- A similar solution by Ciunkos:
sortByHeight' a = zipC a $ sort . filter (>= 0) $ a where
  zipC (a:as) (b:bs)
    | a >= 0    = b : zipC as bs
    | otherwise = a : zipC as (b:bs)
  zipC a      _      = a

{-
Write a function that reverses characters in (possibly nested) parentheses
in the input string, which will always be well-formed with matching ()s.
For inputString = "foo(bar)baz", the output should be "foorabbaz";
For inputString = "foo(bar)baz(blim)", the output should be "foorabbazmilb".
-}

-- ATTEMPT #1 (only works for nested parens :/)

splitAtParens :: [Char] -> ([Char], [Char], [Char])
splitAtParens s = (left, inner, right) where
  isNotParen c    = c /= '(' && c /= ')'
  (left, rest)    = span isNotParen s
  (iRight, iMid)  = span isNotParen (reverse rest)
  (mid, right)    = (reverse iMid, reverse iRight)
  inner           = filter isNotParen mid

reverseInParentheses :: [Char] -> [Char]
reverseInParentheses s
  | hasNoParen s  = s
  | otherwise     = evalParens (splitAtParens s) where
    hasNoParen s = notElem '(' s && notElem ')' s
    evalParens (left, inner, right)
      | hasNoParen inner  = left ++ reverse inner ++ right
      | otherwise         = left ++ innerTrio ++ right where
        innerTrio = reverse . evalParens . splitAtParens $ inner

-- ATTEMPT #2 (works!)

findParens :: [Char] -> [(Int, Int)]
findParens s = foldl findRanges [] (zip s [0..]) where
  findRanges xs ('(', idx) = ((idx, -1):xs)
  findRanges xs (')', idx) = tail xs ++ [(fst (head xs), idx)]
  findRanges xs (_, _)     = xs

splitTwice :: Int -> Int -> [Char] -> ([Char], [Char], [Char])
splitTwice a b s = (x, y1, y2) where
  (x, y)   = splitAt a s
  (y1, y2) = splitAt (b - length x + 1) y

reverseInParentheses' :: [Char] -> [Char]
reverseInParentheses' s =
  filter isAlpha $ foldl revSubList s (findParens s) where
  revSubList s (a, b)   = left ++ reverse inner ++ right where
    (left, inner, right)  = splitTwice a b s

-- Nice solution from `bubbler`
reverseInParentheses'' s = rev s [] where
  rev [] stk      = reverse stk
  rev (')':t) stk = rev t (reverse s1 ++ tail s2)
    where (s1, s2) = span (/='(') stk
  rev (h:t) stk = rev t (h:stk)

{-
Several people in a row need to be divided into two teams.
The first person goes into team 1, the second goes into team 2,
the third goes into team 1, the fourth into team 2, and so on.
Return an array of two integers, where the first is the total
weight of team 1, and the second element is the total weight
of team 2 after the division is complete.
For a = [50, 60, 60, 45, 70], the output should be [180, 105].
-}

alternatingSums :: [Int] -> [Int]
alternatingSums as = map sum alts where
  alts  = foldr inner [[],[]] (zip as [0..])
  inner (a, i) [xs, ys]
    | odd i     = [xs, (a:ys)]
    | otherwise = [(a:xs), ys]

-- Best solution by `vonhyou`
alternatingSums' [] = [0, 0]
alternatingSums' (x:xs) = [x+b, a]
  where [a, b] = alternatingSums xs

{-
Given a rectangular matrix of characters,
add a border of asterisks(*) to it.
For picture = ["abc",
              "ded"]
the output should be = ["*****",
                        "*abc*",
                        "*ded*",
                        "*****"]
-}

addBorder :: [[Char]] -> [[Char]]
addBorder p = e : map (('*':) . (++ ['*'])) p ++ [e]
  where e = replicate (length (head p) + 2) '*'

{-
Two arrays are called similar if one can be obtained from another
by swapping at most one pair of elements in one of the arrays.
Given two arrays a and b, check whether they are similar.
If the arrays are equal, no need to swap any elements (True).
For a = [1, 2, 3] and b = [2, 1, 3], the output should be True.
We can obtain b from a by swapping 2 and 1 in b.
For a = [1, 2, 2] and b = [2, 1, 1], the output should be False.
Any swap either in a or in b won't make a and b equal.
-}

areSimilar :: (Eq a) => [a] -> [a] -> Bool
areSimilar a b = null (a \\ b) && length unequals <= 2
  where unequals = foldl notEqual [] (zip a b)
        notEqual a (x, y) = if x == y then a else (1:a)
