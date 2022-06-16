module Submission where

import DFSM
import Data.Set (Set)
import qualified Data.Set as Set

--given a value and an amount, provides a list of the value in
--the requested quantity
listDup :: (Eq b, Num b) => (a, b) -> [a]
listDup (a, 0) = []
listDup (a, b) = a : listDup (a, b -1)

--returns a list with only the even values of the original
getEvens :: Integral a => [a] -> [a]
getEvens [] = []
getEvens [a] = [a | even a]
getEvens (a : xa) =
  if even a
    then a : getEvens xa
    else getEvens xa

--takes a list and an amount, increases every amount in the
-- list by the given amount and returns the new list
incBy :: Num a => a -> [a] -> [a]
incBy _ [] = []
incBy n [a] = [a + n]
incBy n (a : xa) = (a + n) : (incBy n xa)

--combines the values of 2 lists into one, with element
--representing a value from each list
--if the lists are of different lengths then the resulting list is
--the same length as the sma:ller list
zip1 :: [a] -> [a] -> [(a, a)]
zip1 [] [] = []
zip1 (a : xa) [] = []
zip1 [] (b : xb) = []
zip1 (a : xa) (b : xb) = (a, b) : (zip1 xa xb)

--combines the values of 2 lists into one, with element
--representing a value from each list
--if the lists are of different lengths then the resulting list is
--the same length as the larger list and the vlaues of that
--list are duplicated after the smaller one ends
zip2 :: [a] -> [a] -> [(a, a)]
zip2 [] [] = []
zip2 (a : xa) [] = (a, a) : (zip2 xa [])
zip2 [] (b : xb) = (b, b) : (zip2 [] xb)
zip2 (a : xa) (b : xb) = (a, b) : (zip2 xa xb)

--recursive helper for find, keeps track of index value
findHelper :: (Eq a) => a -> [a] -> Int -> Int
findHelper _ [] _ = (-1)
findHelper n (a : xa) i =
  if n == a
    then i
    else findHelper n xa (i + 1)

--finds a value in a list, if not in the list then returns -1
find :: (Eq a) => a -> [a] -> Int
find n a = findHelper n a 0

--takes an integer and returns the square
sqr :: Int -> Int
sqr x = x * x

--takes two integers and returns the sum
plus :: Int -> Int -> Int
plus x y = x + y

--defines list comprehension, given a function, list, and
-- predicate, returns a list filtered by the predicate
--where the function has been applied to each element
listComp :: (a -> b) -> [a] -> (a -> Bool) -> [b]
listComp _ [] _ = []
listComp f (a : xa) p =
  if p a
    then f a : (listComp f xa p)
    else listComp f xa p

tran =
  [ (0, 'a', 4),
    (0, 'b', 1),
    (1, 'a', 3),
    (1, 'b', 2),
    (2, 'a', 4),
    (2, 'b', 1),
    (3, 'a', 1),
    (3, 'b', 4),
    (4, 'a', 2),
    (4, 'b', 3)
  ]

acc = Set.fromList [1, 2, 3]

eAoB = makeDFSM 0 tran acc
