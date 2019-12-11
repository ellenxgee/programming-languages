module A1c where

-- Assignment 1
-- Ellen Gee

-- sDotProduct
-- The dot product is calculated by multiplying the first coordinate in one pair
--   with the first coordinate in the 2nd pair and adding it to the multiplication
--   of the 2nd coordinate
-- Consume a number n, and two tuples (x1,y1) and (x2,y2)
-- Produce a number r, such that r = ((x1*x2)+(y1*y2))*n
--  sDotProduct 5 (3, 4) (4, 5) -> 160
sDotProduct :: Float -> (Float, Float) -> (Float, Float) -> Float
sDotProduct n (x1,y1) (x2,y2) = ((x1*x2)+(y1*y2))*n

-- distance
-- Consume two pairs
-- Produce the cartesian distance between the pairs
distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1,y1) (x2,y2) = sqrt(((x2-x1)^2)+((y2-y1)^2))

-- tripleDistance
-- Consume two 3-tuples
-- Produce the cartesian distance
tripleDistance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
tripleDistance (x1,y1,z1) (x2,y2,z2) = sqrt(((x2-x1)^2)+((y2-y1)^2)+((z2-z1)^2))

-- findMin
-- Consume a list of numbers
-- Produce the smallest element in the list
findMin :: Ord a => [a] -> a
findMin theList = if (length theList == 0)
                    then error "Empty List"
                    else (if length theList == 1
                      then head theList
                      else (if head theList < findMin (tail theList)
                        then head theList
                        else findMin(tail theList)))

-- tupleDotProduct
-- Consume two lists of numbers
-- Produce the dot product between the lists of numbers
tupleDotProduct :: [Float] -> [Float] -> Float
tupleDotProduct list1 list2 = if (list1 == [])
                                then 0
                                else (head list1 * head list2) + tupleDotProduct (tail list1) (tail list2)

-- revZip2Lists
-- Consume two lists
-- Produce a list with a pair from each list consecutively but in reverse
--  revZip2Lists [1,2,3] ['a','b','c'] -> [('c',3),('b',2),('a',1)]
revZip2Lists :: Eq b => [b] -> [a] -> [(a,b)]
revZip2Lists list1 list2 = if list1 == []
                            then []
                            else (revZip2Lists (tail list1) (tail list2)) ++ [(head list2, head list1)]

-- everyThird
-- Consumes a list of elements
-- Produces a new list only consisting of every third element from the input list
--  everyThird [1,2,3,4,5,6] -> [3,6]
everyThird :: [a] -> [a]
everyThird theList = if (length theList < 3)
                      then []
                      else head(tail(tail theList)) : everyThird (tail(tail(tail theList)))
