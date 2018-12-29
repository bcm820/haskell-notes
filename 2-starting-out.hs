{- FUNCTIONS -}

-- Parameters specified by spaces
-- Def order does not matter (laziness)
doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x

-- Functions can be also invoked using infix notation
-- for cases when it makes more sense to write it this way
doubleUs' x y = x `doubleUs` y

-- 'else' req bc an expr must be eval
-- Apostrophe idiom for strict / modified versions of fns
doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (doubleSmallNumber x) + 1

-- Functions don't start with capital letters
-- A function without parameters is a `definition` or `name`
conanOBrien = "It's a-me, Conan O' Brien!"


{- LISTS / STRINGS -}

-- Lists are homogeneous, composed of head and tail
-- These two are synonymous (nums' approach is sugar)
nums = 4 : 8 : 15 : 16 : 23 : 42 : []
nums' = [4, 8, 15, 16, 23, 42]

-- Strings are sugar for lists of characters
-- A list of chars will print as a string
hello = ['h', 'e', 'l', 'l', 'o']
helloWorld = hello ++ " " ++ "world"

-- `:` prepends a single element as a list's new head
-- `++` appends a list to a list (as its last tail)
-- Note: `++` reqs a list on the right, since it's a tail
aSmallCat = 'A' : " Small Cat"
aSmallCat' = aSmallCat ++ ['!']
aSmallCat'' = aSmallCat ++ "!"

-- `!!` selects an element from a list by index
-- Note an out of bounds index throws an error
selectB = "Steve Buscemi" !! 6 -- 'B'

-- Lists are compared lexiographically
-- Each element is compared across indices
listComparison = [3,2,1] > [2,1,0] -- True
listComparison' = [3,2,1] > [2,10,100] -- True
listComparison'' = [3,4,2] > [3,4] -- True

-- Other basic functions
-- Note: Some throws errors if called on empty list
xs = [5, 4, 3, 2, 1]
xsHead = head xs -- 5
xsTail = tail xs -- [4,3,2,1]
xsLast = last xs -- 1
xsInit = init xs -- [5,4,3,2]
xsLen = length xs -- 5
xsNull = null xs -- False (it is not empty)
xsRev = reverse xs -- [1,2,3,4,5]
xsTake3 = take 3 xs -- [5,4,3]
xsDrop2 = drop 2 xs -- [3,2,1]
xsMin = minimum xs -- 1
xsMax = maximum xs -- 5
xsSum = sum xs -- 15
xsProd = product xs -- 120
xsElem = elem 4 xs -- True (4 is in xs)


{- LIST RANGES -}

-- Lists can be constructed from sequences that can be enumerated
numRange = [1..20]
lowerCharRange = ['a'..'z']
upperCharRange = ['A'..'Z']

-- Steps can also be specified
evenNums = [2, 4..20] -- since 2-4 is 2 steps, skip 2 each time
thirds = [1, 4..20] -- since 1-4 is 3 steps, skip 3 each time
countdown = [10,9..1] -- since 10-9 steps down, count down

{- If you wanted the first 24 multiples of 13, you could
use a normal list, or an infinite list (i.e. stream)
constructed from a range with no upper limit
It is not evaluated until take is called -}
mult13 = [13,26..13 * 24]
mult13' = take 24 [13,26..]

-- Functions that produce infinite lists
xsCycle = take 7 (cycle xs) -- [5,4,3,2,1,5,4]
xsRepeat = take 6 (repeat 5) -- [5,5,5,5,5,5]
-- Note: It's simpler to use `replicate 6 5` for this


{- LIST COMPREHENSIONS -}

-- What if you wanted a list of the first 10 even numbers?
-- i.e. [2,4,6,8,10,12,14,16,18,20]
evens = take 10 [2,4..]

-- You could also use a list comprehension
-- This says: x * 2 for each element x in [1..10]
evens' =
  [ x * 2 |
    x <- [1..10] ]

-- We can also add a predicate to filter the comprehension
evens'' =
  [ x * 2 |
    x <- [1..10],
    x * 2 >= 12 ]


{- What if we wanted a comprehension returned by a fn that
Replaces each odd number greater than 10 with "BANG!"
and each odd number less than 10 with "BOOM!"
and filters out all odd numbers from the list? -}
boomBangs xs =
  [ if x < 10 then "BOOM" else "BANG!" |
    x <- xs,
    odd x ]

-- We can even include several predicates
-- If we wanted numbers 10-20 that are not 13, 15, or 19...
multPreds =
  [ x |
    x <- [10..20],
    x /= 13,
    x /= 15,
    x /= 19 ]

-- We can also draw from several lists rather than just one
-- To get the products of all combinations between two lists...
-- But only products greater than some upper limit...
multLists =
  [ x * y |
    x <- [2,5,10],
    y <- [8,10,11],
    x * y > 50 ]
  -- [55,80,100,110]

-- A list comprehension taht combines adjectives and nouns...
funExample =
  [ a ++ " " ++ n |
    a <- adjectives,
    n <- nouns ]
  where nouns = ["hobo","frog","pope"]
        adjectives = ["lazy","grouchy","scheming"]

-- our own version of length'
-- Given some list, map elements to 1 and sum the list
length' xs = sum [ 1 | _ <- xs ]

-- Remove all non-uppercase characters from a string
removeNonUpper str = [ c | c <- str, elem c ['A'..'Z'] ]

-- Nested list comprehensions are possible on nested lists
-- For example, to remove all odd numbers from this 2d list:
xxs =
  [ [1,3,5,3,1,2,4],
    [1,2,3,4,5,6,7],
    [1,2,4,2,1,3,5] ]
xxsEvens = [ filter even xs | xs <- xxs ] -- [[2,4],[2,4,6],[2,4,2]]
xxsEvens' = -- not as readable, but demonstrates nested list comps
  [ [ x |
      x <- xs, even x ] |
      xs <- xxs ]


{- TUPLES -}

-- Tuples are data collections like lists, but are not homogeneous.
-- They are used when you know exactly how many values to combine.
-- Tuple are typed according to their values (i.e. (Integer, Integer) )
twoDVector' = [ [1,2], [3,4], [5,6,7] ]
twoDVector'' = [ (1,2), (3,4), (5,6) ] -- can't add 7 here

-- Useful functions for tuple pairs
firstComp = fst (8, 11) -- 8
secondComp = snd (8, 11) -- 11

-- zip takes two lists and combines them into a list of pairs
leftZip = "ABC"
rightZip = [1, 2, 3, 4] -- Note: 4 will be left out here
zipped = zip leftZip rightZip -- [('A',1),('B',2),('C',3)]

-- since Haskell is lazy, we can zip finite lists and infinite lists
zipFruits = zip [1..] ["apple", "orange", "cherry", "mango"]
-- [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]

-- Given triangle sides of varying lengths <= 10,
-- determine the right triangle combination
-- where the perimeter is exactly 24.
rightTriangle =
  let oneToTen = [1..10]
  in [ (a, b, c) |
    a <- oneToTen,
    b <- oneToTen,
    c <- oneToTen,
    a^2 + b^2 == c^2,
    a + b + c == 24 ]
  -- [(6, 8, 10), (8, 6, 10)]

{- This is a common pattern in FP.
You take a starting set of solutions,
apply transformations to those solutions
and filter until you get the right ones. -}
