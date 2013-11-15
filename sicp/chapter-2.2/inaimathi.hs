---- 2.17 ----------
lastPair :: [a] -> [a]
lastPair [elem] = [elem]
lastPair (first:rest) = lastPair rest

---- 2.18 ----------
myReverse :: [a] -> [a]
myReverse lst = rec lst []
  where rec [] acc = acc
        rec (first:rest) acc = rec rest $ first:acc
        
---- 2.19 ----------
-- You actually wouldn't want these selectors in Haskell.
-- As shown above, the idiomatic way of getting the head of a list
-- is to destructure it in the argument.
-- In particular, `head` should be avoided because it's not complete.
-- That is

--	*Main> head []
--      *** Exception: Prelude.head: empty list
--      *Main> 

---- 2.20 ----------

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter predicate list = rec list []
  where rec [] acc = myReverse acc
        rec (a:rest) acc
          | predicate a = rec rest $ a:acc
          | otherwise = rec rest acc
                        
sameParity :: [Int] -> [Int]
sameParity lst@(num:rest)
  | even num = myFilter even lst 
  | otherwise = myFilter odd lst
                
---- 2.21 ----------
square :: Int -> Int
square n = n*n

squareList :: [Int] -> [Int]
squareList = map square

---- 2.23 ----------
-- This is slighlty different. Since Haskell is pure by default, something like 
-- forEach would only really make sense in an IO context, so...
forEach :: (a -> IO b) -> [a] -> IO b
forEach proc (val:[]) = do 
  proc val
forEach proc (val:rest) = do
  proc val
  forEach proc rest
