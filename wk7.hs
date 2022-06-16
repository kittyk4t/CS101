module Submission where

data IntTree = Leaf Integer | Interior (IntTree, IntTree) deriving (Show)

treeSum :: IntTree -> Integer
treeSum (Leaf n) = n
treeSum (Interior (lt, rt)) = treeSum lt + treeSum rt

heightHelp :: IntTree -> Integer -> Integer
heightHelp (Leaf n) h = h
heightHelp (Interior (lt, rt)) h =
  if heightHelp lt h > heightHelp rt h
    then heightHelp lt h + 1
    else heightHelp rt h + 1

height :: IntTree -> Integer
height t = heightHelp t 0

balanced :: IntTree -> Bool
balanced (Leaf n) = True
balanced (Interior (lt, rt)) = abs (height lt - height rt) <= 1