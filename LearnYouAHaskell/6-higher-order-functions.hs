{-
Every function in Haskell only takes one parameter.
A space between two 'parameters' is really fn application.
Not specifying all 'parameters' creates a partially applied fn.
-}
addThree :: (Num a) => a -> a -> a -> a
addThree a b c = a + b + c

addOneMore = addThree 1 2 -- :t addOneMore :: Integer -> Integer
addedThree = addOneMore 3 -- 6

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
compareWithHundred' = compare 100
  -- same thing, since compare expects another argument to be given

-- Infix functions can also be partially applied
-- by surrounding it in parens and supplying only one arg
divideByTen :: (Floating a) => a -> a
divideByTen = ( / 10)


{- HIGHER ORDER FUNCTIONS -}

-- This fn receives a fn, something, and applies the fn twice
-- The parens indicates the first parameter is a fn
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- If a fn requires a fn that takes only one argument,
-- we can partially apply a fn to the point where it
-- only takes one argument, and then pass it
apTwiceAddThree = applyTwice (addThree 2 2) 9 -- 144

-- `zipWith` joins two lists by applying some fn
-- It receives a fn, two lists, and returns a new list
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = [] -- when either list is empty
zipWith' _ _ []          = [] -- stop zipping!
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- higher order fns can be used to abstract away common patterns
-- like combining two lists into pairs
zipAdd = zipWith' (+) [4,2,5,6] [2,6,2,3] -- [6,8,7,9]
zipMax = zipWith' max [6,3,2,1] [7,3,1,5] -- [7,3,2,5]
zipMin = zipWith' min [6,3,2,1] [7,3,1,5] -- [6,3,1,1]
zipLists = zipWith' (++) ["foo ", "bar "] ["fighters", "hoppers"]
zipMult = zipWith' (*) (replicate 5 2) [1..] -- [2,4,6,8,10]

-- `flip` takes a fn with two params and flips the params order
-- Note the type declaration (the 2nd parens are unnecessary)
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- `map`
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map f xs

-- `filter`
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

-- `quicksort` using filter
-- Thanks to Haskell's laziness, if you map and filter over a list
-- several times, it will only pass over the list once.
quicksort []     = []
quicksort (x:xs) = quicksort (filter' (<=x) xs)
                   ++ [x] ++
                   quicksort (filter' (>x) xs)

-- Find the largest number under 100,000 that's divisible by 3829
-- We first make a list of all numbers lower than 100,000, descending.
-- Then we filter, and the largest number that satisfies our predicate
-- is the first element of the filtered list.
-- Because we only end up using the head of the filtered list,
-- it doesn't matter if the filtered list is finite or infinite.
-- The evaluation stops when the first adequate solution is found.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- Find the sum of all odd squares smaller than 10,000
-- Again, Haskell's laziness makes this possible
sumOfOddSquares = sum (takeWhile (<10000) [ n^2 | n <- [1..], odd (n^2) ])

-- Collatz sequences take a natural number which,
-- if even, divide by two; if odd, divide by 3 and add 1;
-- the resulting number is then applied to the fn again
-- If the number is 1, however, return 1 and end the chain.
collatz :: (Integral a) => a -> [a]
collatz n
  | n == 1 = [1]
  | even n = n : collatz (div n 2)
  | odd n = n : collatz (n*3 + 1)

-- Given all Collatz sequences starting from 1 to 100,
-- how many have lengths greater than 15 numbers?
numLongChains :: Int
numLongChains =
  let isLong xs = length xs > 15
  in length (filter isLong (map collatz [1..100]))


{- LAMBDAS -}

-- The `\` kinda looks like the Greek letter lambda
numLongChains' =
  length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- Lambdas can take any number of parameters
twoParams = zipWith (\a b -> (a * 3) / b) [1,2,3] [1,2,3]

-- Lambda params can be pattern matched
pMatch = map (\(a,b) -> a + b) [(1,2),(3,4)]


{-
FOLDS
Folds implement the common recursive pattern on lists
-}

-- `foldl`: Left fold combines the list from the left side
sum' :: (Num a) => [a] -> a
sum' = foldl (\acc x -> acc + x) 0

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- `foldr`: Right fold accumulates from right to left
-- When building up new lists from a list, it's best to do foldr
-- since prepending to a list (new head) is more efficient than :
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- Also, `foldr` works on infinite lists, whereas left ones don't
-- If you `take` from an infinite list and fold right
-- you can reach the beginning of the list.
-- But start from the left and you'll never reach the end.

-- `foldl1` and `foldr1` assumes first value to be the initial value
-- `sum` is more easily implemented using it
sum''' :: (Num a) => [a] -> a
sum''' = foldl1 (+)

-- Many stdlib fns can be implemented using folds
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 max

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) [] -- x : acc

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

-- `scanl` and `scanr` (and `scanl1` and `scanr1`) are like folds
-- but they store the immediate accumulator states in a list
-- scanl puts the final result at the end, while scanr in the head
s1 = scanl (+) 0 [3,5,2,1] -- [0,3,8,10,11]
s2 = scanr (+) 0 [3,5,2,1] -- [11,8,3,1,0]
s3 = scanl1 max [3,4,5,3,7,9,2] -- [3,4,5,5,7,9,9]
s4 = scanl (flip (:)) [] [3,2,1] -- [[],[3],[2,3],[1,2,3]]

{-
How many elements does it take for the sum of the square roots
of all natural numbers to exceed 1000?
To find out, scan the infinite list of natural number sqrts,
and see how many sums are under 1000.
Note that `filter` does not work on infinite lists
since it has to test each element in the list.
-}
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


{-
FUNCTION APPLICATION & COMPOSITION
`$` - right-associative function application
`.` - right-associative function composition
-}

-- Instead of using parentheses...
-- i.e. sqrt (3 + 4 + 9)
ex1 = sqrt $ 3 + 4 + 9

-- Pass an arg into a list of functions
ex2 = map ($3) [(4+), (10*), (^2)] -- [7.0, 30.0, 9.0]

-- Instead of using lambdas (all the time)
-- i.e. map (\x -> negate (abs x)) [5,-7,2]
ex3 = map (negate . abs) [5,-7,2] -- [-5,-7,-2]

-- When composing fns that have several parameters,
-- left-associative fn composition requires many parens.
-- We can partially apply such fns so that each takes 1 arg.

-- i.e. sum (replicate 5 (max 6.7 8.9))
ex4 = sum . replicate 5 . max 6.7 $ 8.9
-- replicate 5 times the max between 6.7 and some n (8.9 here)
-- then with this new list of 5, sum the results

-- i.e. 6 + (product (map (*3) (zipWith (+) [1,2,3] [4,5,6])))
ex5 = (+6) . product . map (*3) . zipWith (+) [1,2,3] $ [4,5,6]
-- zip [1,2,3] and [4,5,6] by addition
-- multiply each element of the list by 3
-- find the product of the list and then add 6

-- Writing in point-free style
-- i.e. Instead of `ex6 someX = ceiling (negate (tan (cos (max 50 someX))))`
-- someX is used on both sides, so we can leave out someX from both sides.
-- but Haskell won't understand `max 50` on its own... unless...
ex6 = ceiling . negate . tan . cos . max 50
-- Now just give ex6 a number, and it will...
-- Find the max between that number and 50
-- apply cos, then tan, then negate, then ceiling...

{-
Many times, point free style can be less readable if too complex.
The prefered style is to use let bindings to give labels
to intermediary results or split the problem into sub-problems
and then put it together so that the function makes sense.
-}
