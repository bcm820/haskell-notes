{- PATTERN MATCHING -}

-- For any function, you can define distinct function bodies
-- based on the pattern of the arguments given
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- Imagine how convulted this would be using if-then-else
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3"

-- We can define factorial recursively, as in math
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Here's the standard definition of Fibonacci
fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Pattern matching can be used on tuples
-- You can destructure the tuple to get its values
-- Note two ways to implement `addVectors`
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b                = (fst a + fst b, snd a + snd b) -- original
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) -- pattern matching
-- Note this is a catch-all pattern based on the type signature
-- Note also that the pattern matching is done on the actual parameters

-- Pattern matching can be used in list comprehensions
xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
meld = [ a + b | (a, b) <- xs ]

-- Lists themselves can be pattern matched
head' :: [a] -> a
head' []    = error "no head for empty list"
head' (x:_) = x

-- Note how the patterns for a list are all exhausted
tell :: (Show a) => [a] -> String
tell []      = "empty"
tell [x]     = "singleton" ++ show x
tell [x,y]   = "two elements" ++ show x ++ " and " ++ show y
tell (x:y:_) = "longer..." ++ show x ++ " and " ++ show y

-- length via pattern matching and recursion
length' :: (Num b) => [a] -> b
length' []     = 0
length' (_:xs) = 1 + length' xs

-- sum
sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- `as patterns` can be used to retain a reference
-- while at the same time breaking it up by pattern
capital :: String -> String
capital ""         = "Empty string, whoops!"
capital word@(x:_) = "The first letter of " ++ word ++ " is " ++ [x]


{- GUARDS -}

-- Guards test whether some properties of a value are true or false
-- They are indicated by pipes following a function name and params
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight!"
  | bmi <= 25.0 = "You're supposedly normal!"
  | bmi <= 30.0 = "You're fat! Lose some weight!"
  | otherwise   = "You're a whale!"

-- You can use guards with multiple parameters
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight!"
  | otherwise                   = "You're a whale!"

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
  | a > b     = GT
  | a < b     = LT
  | otherwise = EQ


{- WHERE -}

-- `where` bindings declare variables at the end of a function
-- and the whole function can see them, including the guards.

-- `bmiTell'` can be refactored down using `where`
bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
  | bmi <= 18.5 = "You're underweight!"
  | bmi <= 25.0 = "You're supposedly normal!"
  | bmi <= 30.0 = "You're fat! Lose some weight!"
  | otherwise   = "You're a whale!"
  where bmi = weight / height ^ 2

-- We could go further and define more names...
-- Notice use of bindings to pattern match
bmiTell''' :: (RealFloat a) => a -> a -> String
bmiTell''' weight height
  | bmi <= skinny = "You're underweight!"
  | bmi <= normal = "You're supposedly normal!"
  | bmi <= fat = "You're fat! Lose some weight!"
  | otherwise   = "You're a whale!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        (normal, fat) = (25.0, 30.0)

-- Another example using `where`
-- Note the simpler implementation on the bottom
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

-- In addition to defining constants in `where` blocks,
-- you can also define functions
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
  where bmi weight height = weight / height ^ 2


{- LET -}

-- `let` bindings declare variables anywhere in a function,
-- but they are local and don't span across guards.
-- The form is `let <bindings> in <expression>`
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in  sideArea + 2 * topArea

-- `let` bindings are expressions themselves
-- which means they can be used locally anywhere
-- an expression needs to be evaluated
fourtyTwo = 4 * (let a = 9 in a + 1) + 2 -- 42
fnResults = [let sq x = x * x in (sq 5, sq 3, sq 2)] -- [(25,9,4)]
letSemicolons = (let a = 100; b = 200; c = 300 in a + b + c) -- 600
letPatternMatch = (let (a,b,c) = (1,2,3) in a + b + c) -- 600

-- `let` bindings can go inside of list comprehensions
-- The bindings are added prior to including a predicate
-- Note bindings are only used in the output fn and the predicate
getFatBmis :: (RealFloat a) => [(a, a)] -> [a]
getFatBmis xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0 ]

-- We can use `let` bindings within a list comp's predicate
-- so that it is only visible to the predicate
getSkinnyBmis :: (RealFloat a) => [(a, a)] -> [a]
getSkinnyBmis xs =
  [ bmi | (w, h) <- xs,
    let bmi = w / h ^ 2,
    let skinny = 25.0 in bmi >= skinny ]


{- CASE EXPRESSIONS -}

-- Case syntax in Haskell allows for pattern matching
-- In fact, pattern matching on params is sugar for case expressions
head'' :: [a] -> a
head'' xs = case xs of []    -> error "No head for empty lists!"
                       (x:_) -> x

-- While pattern matching on fn params can only be done when defining fns,
-- case expressiosn can be used anywhere
describeList :: [a] -> String
describeList xs = "The list is " ++
  case xs of []  -> "an empty list."
             [x] -> "a singleton list."
             xs  -> "a longer list."

-- The syntactic sugar version of this would be:
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where what []  = "an empty list"
        what [x] = "a singleton list."
        what xs  = "a longer list."
