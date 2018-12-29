{- TYPES -}

{- Common Types:
- Int
- Integer
- Float
- Double
- Bool
- Char  -}

-- Integer is used for big numbers (unbounded)
factorial :: Integer -> Integer
factorial n = product [1..n]
-- factorial 20 -> 2432902008176640000

-- While Haskell has type inference,
-- it's good practice to specify type for a function.
removeNonUppercase :: [Char] -> [Char] -- or String
removeNonUppercase str = [ c | c <- str, elem c ['A'..'Z'] ]

-- Multiple parameters are separated by arrows
-- In Haskell, functions are automatically curried
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Functions also have type variables
-- To specify a type variable, write the type in lowercase
-- Functions with type variables are polymorphic functions
head' :: [a] -> a
head' anyList = head anyList

fst' :: (a, b) -> a
fst' tuple = fst tuple


{- TYPECLASSES -}

-- A typeclass is an interface that defines some behavior
-- Type signature of equals function:
equals' :: (Eq a) => a -> a -> Bool
equals' x y = x == y
-- The `(Eq a) =>` specifies a class constraint
-- It says the type `a` must be a member of the Eq class.

{- Basic Typeclasses:
- Eq: supports equality tests (implements == and =/)
- Ord: has an ordering (can compare via >, <, >=, <=)
- Show: can be presented as strings (i.e. via `show`)
- Read: can be read from a given string
- Enum: can be enumerated (i.e. in list ranges)
- Bounded: has an upper and lower bound (i.e. Int)
- Num: have the behavior of numbers (i.e. Int, Integer)
- Integral: has the behavior of WHOLE numbers
- Floating: has the behavior of floating point numbers  -}

-- Return types can be declared when calling a function
-- Specifying a return type determines a function's output
five = read "5" :: Int -- 5
five' = read "5" :: Float -- 5.0
five'' = read "[1,2,3,4,5]" :: [Int] -- [1,2,3,4,5]
five''' = read "(5, 5.0)" :: (Int, Float) -- (5, 5.0)

-- A useful example for when dealing with numbers:
-- `fromIntegral :: (Num b, Integral a) => a -> b`
-- fromIntegral turns an integral number into a general number
-- since `length` returns an integer, you can't add it to a float
-- unless you use fromIntegral to generalize it to a Num.
lenPlusFloat' = fromIntegral (length [1,1,1]) + 3.2 -- 6.2
