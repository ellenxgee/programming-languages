module A2b where
  
-- Assignment 2
-- Ellen Gee

import Data.Maybe

-- removeAllExcept
-- Consume an element n and a list of elements that are the same type as n
-- Produce a list that has removed everything that is not equal to the given element n from the initial list
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept n [] = []
removeAllExcept n (x:xs) = if n == x
                              then x : removeAllExcept n xs
                              else removeAllExcept n xs

-- removeAll
-- Consume an element n and a list of elements that are the same type as n
-- Produce a list that has removed all occurrences of the given element n
removeAll :: Eq a => a -> [a] -> [a]
removeAll n [] = []
removeAll n (x:xs) = if n /= x
                        then x : removeAll n xs
                        else removeAll n xs

-- substitute
-- Consume two elements n and m and a list of elements that are the same type as n and m
-- Produce a list that has switched each element in the initial list equal to n to m
substitute :: Eq a => a -> a -> [a] -> [a]
substitute n m [] = []
substitute n m (x:xs) = if x == n
                            then m : substitute n m xs
                            else x : substitute n m xs

-- mergeSorted3
-- Consume three lists with elements of the same type in sorted order
-- Produce a list of the three input lists merged and sorted in increasing order
--  mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10] is [-1, 0, 1, 2, 3, 4, 5, 8, 10]
mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 [] [] [] = []
mergeSorted3 (x:xs) [] [] = x:xs
mergeSorted3 [] (y:ys) [] = y:ys
mergeSorted3 [] [] (z:zs) = z:zs
mergeSorted3 (x:xs) (y:ys) [] = if (x <= y)
                                  then x : mergeSorted3 xs (y:ys) []
                                  else y : mergeSorted3 (x:xs) ys []
mergeSorted3 [] (y:ys) (z:zs) = if (y <= z)
                                  then y : mergeSorted3 [] ys (z:zs)
                                  else z : mergeSorted3 [] (y:ys) zs
mergeSorted3 (x:xs) [] (z:zs) = if (x <= z)
                                  then x : mergeSorted3 xs [] (z:zs)
                                  else z : mergeSorted3 (x:xs) [] zs
mergeSorted3 (x:xs) (y:ys) (z:zs) = if (x <= y && x <= z)
                                      then (x : (mergeSorted3 xs (y:ys) (z:zs)))
                                      else (if (y <= x && y <= z)
                                            then (y : (mergeSorted3 (x:xs) ys (z:zs)))
                                            else (if (z <= x && z <= y)
                                                    then (z : (mergeSorted3 (x:xs) (y:ys) zs))
                                                    else []))

-- create a TriTree object
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a )
     deriving (Ord, Show)
instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False

-- nodeValue
-- Consume a TriTree
-- Produce the value of the TriTree
nodeValue :: TriTree a -> a
nodeValue EmptyNode = error "Calling on an empty node"
nodeValue (TriNode a la ma ra) = a

-- leftChild
-- Consume a TriTree
-- Produce a TriTree which is the left child of the input TriTree
leftChild :: TriTree a -> TriTree a
leftChild EmptyNode = error "Calling on an empty node"
leftChild (TriNode a la ma ra) = la

-- middleChild
-- Consume a TriTree
-- Produce a TriTree which is the middle child of the input TriTree
middleChild :: TriTree a -> TriTree a
middleChild EmptyNode = error "Calling on an empty node"
middleChild (TriNode a la ma ra) = ma

-- rightChild
-- Consume a TriTree
-- Produce a TriTree which is the right child of the input TriTree
rightChild :: TriTree a -> TriTree a
rightChild EmptyNode = error "Calling on an empty node"
rightChild (TriNode a la ma ra) = ra

-- inTree
-- Consume an element and a TriTree
-- Produce a Boolean, True if the element is in TriTree, False if it isn't
inTree :: Eq a => a -> TriTree a -> Bool
inTree n EmptyNode = False
inTree n (TriNode a la ma ra) = if (a == n)
                                    then True
                                    else if inTree n la
                                          then True
                                          else if inTree n ma
                                                  then True
                                                  else if inTree n ra
                                                        then True
                                                        else False

-- leafList
-- Consume a TriTree
-- Produce a list of all the values in the leaves of the tree
leafList :: TriTree a -> [a]
leafList EmptyNode = []
leafList (TriNode a EmptyNode EmptyNode EmptyNode) = [a]
leafList (TriNode a la ma ra) = (leafList la) ++ (leafList ma) ++ (leafList ra)

-- inOrderMap
-- Consume a function and a TriTree
-- Produce a TriTree with the same structure but each value transformed by the function
inOrderMap :: (a -> b) -> TriTree a -> TriTree b
inOrderMap f EmptyNode = EmptyNode
inOrderMap f (TriNode a la ma ra) = TriNode (f a) (inOrderMap f la) (inOrderMap f ma) (inOrderMap f ra)

-- preOrderFold
-- Consume a function, an accumulator value, and a TriTree
-- Produce an pre-order walk of the tree, applying the function to each value and
--  then using the result of that function in the next call of the folding in the tree
preOrderFold:: (b -> a -> b) -> b -> TriTree a -> b
preOrderFold f acc EmptyNode = acc
preOrderFold f acc (TriNode a la ma ra) = preOrderFold f (preOrderFold f (preOrderFold f (f acc a) la) ma) ra
