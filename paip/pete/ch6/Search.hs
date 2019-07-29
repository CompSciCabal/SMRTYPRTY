{-# LANGUAGE ScopedTypeVariables #-}

module Search where

import           Data.Function
import           Data.List
import           Data.Set      (Set)
import qualified Data.Set      as Set

import           Debug.Trace

treeSearch :: Show a => [a] -> (a -> Bool) -> (a -> [a]) -> ([a] -> [a] -> [a]) -> Maybe a
treeSearch startStates isGoal successors combine =
  search startStates
    where
      search states =
        case traceShowId states of
          [] -> Nothing
          x:xs -> if isGoal x then Just x else search (combine (successors x) xs)

-- graphSearch startStates isGoal successors combine
-- attempts to find a state that satisfies isGoal,
-- starting with startStates, and searching according
-- to successors and combine.  It does not try the same
-- state twice.
graphSearch :: (Ord a, Show a) => [a] -> (a -> Bool) -> (a -> [a]) -> ([a] -> [a] -> [a]) -> Maybe a
graphSearch startStates isGoal successors combine =
  search startStates (Set.fromList startStates)
    where
      search states seen =
        case traceShowId states of
          [] ->
            Nothing
          x:xs | isGoal x ->
            Just x
          x:xs | otherwise ->
            let stepStates = filter (\y -> Set.notMember y seen') (successors x)
                seen' = Set.insert x seen
                states' = nub (combine (successors x) xs)
             in search states' seen'

depthFirstSearch :: Show a => a -> (a -> Bool) -> (a -> [a]) -> Maybe a
depthFirstSearch start isGoal successors =
  treeSearch [start] isGoal successors (++)

breadthFirstSearch :: Show a => a -> (a -> Bool) -> (a -> [a]) -> Maybe a
breadthFirstSearch start isGoal successors =
  treeSearch [start] isGoal successors prepend

prepend = flip (++)

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

next2 :: Integer -> [Integer]
next2 n = [n+1, n+2]
