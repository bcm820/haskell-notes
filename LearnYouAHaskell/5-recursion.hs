{-
In Haskell, we declare what something is
rather than declare how you get it
Recursion lets us declare a function's
pure output given a variety of inputs
-}

-- The maximum of a singleton list is its element
-- The maximum of a longer list is...
-- The max of the head and maximum of the tail
-- `maximum t` will be called recursively
-- returning either head or maximum of tail each time
maximum' :: (Ord a) => [a] -> a
maximum' []    = error "max of empty list"
maximum' [h]   = h -- edge condition
maximum' (h:t) = max h (maximum' t)

-- `replicate`
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n - 1) x

-- `take`
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ []     = []
take' n _      | n <= 0 = []
take' n (x:xs) = x : take' (n - 1) xs

-- `reverse`
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- `repeat` (returns an infinite list)
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- `zip`
zip' :: [a] -> [b] -> [(a,b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- `elem`
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

-- `quicksort`
-- a sorted list has all values LTE the head,
-- then comes the head,
-- then all the values GTE the head
quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort [ a | a <- xs, a <= x ]
                   ++ [x] ++
                   quicksort [ a | a <- xs, a > x ]
