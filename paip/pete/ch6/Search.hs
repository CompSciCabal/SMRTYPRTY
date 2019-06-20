module Search where

import Data.List
import Data.Function

import Debug.Trace

treeSearch :: Show a => [a] -> (a -> Bool) -> (a -> [a]) -> ([a] -> [a] -> [a]) -> Maybe a
treeSearch startStates isGoal successors combine =
  search startStates
    where
      search states =
        case traceShowId states of
          [] -> Nothing
          x:xs -> if isGoal x then Just x else search (combine (successors x) xs)

depthFirstSearch :: Show a => a -> (a -> Bool) -> (a -> [a]) -> Maybe a
depthFirstSearch start isGoal successors =
  treeSearch [start] isGoal successors (++)

breadthFirstSearch :: Show a => a -> (a -> Bool) -> (a -> [a]) -> Maybe a
breadthFirstSearch start isGoal successors =
  treeSearch [start] isGoal successors (flip (++))

bestFirstSearch :: (Show a, Ord b) => a -> (a -> Bool) -> (a -> [a]) -> (a -> b) -> Maybe a
bestFirstSearch start isGoal successors cost =
  treeSearch [start] isGoal successors (sorter cost)

sorter :: (Show a, Ord b) => (a -> b) -> [a] -> [a] -> [a]
sorter cost xs ys = sortBy (compare `on` cost) (xs ++ ys)

beamSearch :: (Show a, Ord b) => Int -> a -> (a -> Bool) -> (a -> [a]) -> (a -> b) -> Maybe a
beamSearch k start isGoal successors cost =
  treeSearch [start] isGoal successors combine
    where
      combine xs ys = take k (sorter cost xs ys)

bestFirstBinarySearch :: (Integer -> Integer -> Integer) -> Integer -> Maybe Integer
bestFirstBinarySearch cmp n = bestFirstSearch 1 (== n) binaryTree (cmp n)

beamBinarySearch :: (Integer -> Integer -> Integer) -> Int -> Integer -> Maybe Integer
beamBinarySearch cmp k n = beamSearch k 1 (== n) binaryTree (cmp n)

diff :: Integer -> Integer -> Integer
diff a b = abs (a-b)

priceIsRight :: Integer -> Integer -> Integer
priceIsRight a b = if a < b then 10^20 else a - b

binaryTree :: Integer -> [Integer]
binaryTree n = [2*n, 2*n+1]

finiteBinaryTree :: Integer -> Integer -> [Integer]
finiteBinaryTree sup n = filter (< sup) (binaryTree n)

