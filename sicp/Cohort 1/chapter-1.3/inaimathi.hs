module SICP where

-- Basics that we aren't asked to define, but are needed by other answers
square a = a * a
isPrime k = null [ x | x <- [2..k -1], k `mod` x == 0]

--- 1.29

-- o_o'

--- 1.30
sum :: (Ord a, Num a) => (a -> a) -> a -> (a -> a) -> a -> a
sum term a next b = iter a 0
  where iter a res
          | a > b    = res
          | otherwise = iter (next a) $ res + term a
                        
-- However, Haskell being lazy, the above performs no better than

sumRec :: (Ord a, Num a) => (a -> a) -> a -> (a -> a) -> a -> a
sumRec term a next b
  | a > b     = 0
  | otherwise = (term a) + (sumRec term (next a) next b)

--- 1.31
sequence :: (Ord a, Num a) => (a -> a -> a) -> (a -> a) -> a -> (a -> a) -> a -> a
sequence combine term a next b = iter a 0
  where iter a res
          | a > b     = res
          | otherwise = iter (next a) . combine res $ term a

prod = SICP.sequence (*)

--- 1.32
-- Heh. Already defined in 1.31, but I mis-named it

accumulate :: (Ord a, Num a) => (a -> a -> a) -> (a -> a) -> a -> (a -> a) -> a -> a
accumulate combine term a next b = iter a 0
  where iter a res
          | a > b     = res
          | otherwise = iter (next a) . combine res $ term a

--- 1.33
filteredAccumulate :: (Ord a, Num a) => (a -> Bool) -> (a -> a -> a) -> (a -> a) -> a -> (a -> a) -> a -> a
filteredAccumulate filter combine term a next b = iter a 0
  where iter a res
          | a > b     = res
          | filter a  = iter (next a) . combine res $ term a
          | otherwise = iter (next a) res

-- a)
sumPrimeSquares a b = filteredAccumulate isPrime (+) square a succ b

-- b)
abelsonsSum n = filteredAccumulate ((==1) . (gcd n)) (*) id 1 succ $ succ n

-- Now then, as promised, what I see as the natural decompositions to the things we're writing.

sumPrimeSquaresNat a b = Prelude.sum . map square $ filter isPrime [a..b]

abelsonsSumNat n = product $ filter ((==1) . (gcd n)) [1..n]

-- No filtered accumulate garbage, just string them together properly and you get
-- a fairly simple and straightforward expression of the underlying computations.
--   Granted, Haskell helps here by being lazy. Specifically, that means we don't have the
-- same constant-time overhead here that we would in Scheme if we decided to work with
-- lists rather than tail recursions.