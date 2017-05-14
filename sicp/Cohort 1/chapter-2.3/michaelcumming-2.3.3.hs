--Exercise 2.59
--Set as unordered list
newtype Set a = Set [a]
	deriving (Show, Eq)

--unordered list version
elemSet :: Eq a => a -> (Set a) -> Bool
elemSet x (Set []) 	= False
elemSet x (Set (y:ys))
	| x==y 		= True
	| otherwise = elemSet x (Set ys)

set1 = (Set [1, 4, 6, 7, 8, 9])
set2 = (Set [0, 4, 3, 7, 8, 10])

--unordered list version
unionSet :: Eq a => (Set a) -> (Set a) -> (Set a)
unionSet (Set xs) (Set ys) = Set (uni xs ys)
uni :: Eq a => [a] -> [a] -> [a]
uni (x:xs) [] = (x:xs)
uni [] ys = ys
uni (x:xs) ys
	--if x is already in the other set, don't add it
	| elemSet x (Set ys) = uni xs ys
	| otherwise          = x : uni xs ys

--Exercise 2.60
--Set as unordered list
newtype Set a = Set [a]
	deriving (Show, Eq)

--unordered list version, duplicates allowed
elemSet :: Eq a => a -> (Set a) -> Bool
elemSet x (Set []) 	= False
elemSet x (Set (y:ys))
	| x==y 			= True
	| otherwise 	= elemSet x (Set ys)

test1 = (Set [1, 4, 6, 7, 8, 9])
test2 = (Set [0, 4, 3, 7, 8, 10])

--unordered list version, duplicates allowed
adjoinSet :: Eq a => a -> (Set a) -> (Set a)
adjoinSet x (Set xs) = Set (x : xs)

--unordered list version, duplicates allowed
unionSet :: Eq a => (Set a) -> (Set a) -> (Set a)
unionSet (Set xs) (Set ys) = Set (uni xs ys)
uni :: Eq a => [a] -> [a] -> [a]
uni (x:xs) [] = (x:xs)
uni [] ys     = ys
uni (x:xs) ys = x : uni xs ys --add everything

interSet :: Eq a => (Set a) -> (Set a) -> (Set a)
interSet (Set xs) (Set ys) = Set (inter xs ys)
inter :: Eq a => [a] -> [a] -> [a]
inter (x:xs) [] = []
inter [] ys = []
inter (x:xs) ys
	--if it's in the other set then cons it to the result
	| elemSet x (Set ys) = x : inter xs ys
	| otherwise          = inter xs ys

--??Efficiency: elemSet=same: O(n); adjoinSet=same: O(n); unionSet=better: O(n); interSet=name: O(n2);

--??Applications: when data is small and storage costs are not an issue; when duplication stats adds useful information; when having duplicates speeds up access

--Exercise 2.61
--ordered list version
adjoinSet :: Ord a => a -> [a] -> [a]
adjoinSet x [] = [x]
adjoinSet x (y:ys)
	| x<y       = x:(y:ys)
	| x==y 		= (y:ys)
	| otherwise = y : adjoinSet x ys

--Exercise 2.62
--ordered list version
unionSet :: Ord a => [a] -> [a] -> [a]
unionSet (x:xs) [] = (x:xs)
unionSet [] (y:ys) = (y:ys)
unionSet (x:xs) (y:ys)
	| x<y       = x : unionSet xs (y:ys)
	| x==y      = x : unionSet xs ys
	| otherwise = y : unionSet (x:xs) ys


--this is as far as I got...

--Exercise 2.63 --incomplete
--ordered list version
unionSet :: Ord a => [a] -> [a] -> [a]
unionSet (x:xs) [] = (x:xs)

--Exercise 2.65
--binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) 
	deriving (Show, Read, Eq)  

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
      
--FYI this is what a binary tree looks like in Haskell
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node y left right)   
    | x==y = Node y left right  
    | x<y  = Node y (treeInsert x left) right  
    | x>y  = Node y left (treeInsert x right)  