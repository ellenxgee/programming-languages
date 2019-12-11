module A3b where

-- Ellen Gee
-- Assignment 3

import Data.List
import Data.Char
import Data.Maybe

-- onlyLowercase
-- Consume a String list
-- Produce a string list that only has the strings in the given list that begin
--  with a lower-case letter
onlyLowercase :: [String] -> [String]
onlyLowercase [] = []
onlyLowercase (x:xs) = filter (\x -> isLower (head x)) (x:xs)

-- longestString
-- Consume a list of strings
-- Produce the longest string in the list, if there is a tie, the first one is returned
longestString :: [String] -> String
longestString [] = ""
longestString xs = foldl (\x y -> if length x >= length y then x else y) [] xs

-- longestString'
-- Consume a list of strings
-- Produce the longest string in the list, if there is a tie, the last one is returned
longestString' :: [String] -> String
longestString' [] = ""
longestString' xs = foldl (\x y -> if length x > length y then x else y) [] xs

-- longestStringHelper
-- Consume a function and a list of strings
-- Produce a string chosen by the function
longestStringHelper :: (Int -> Int -> Bool) -> [String] -> String
longestStringHelper f [a] = a
longestStringHelper f (a:b:cs) = if (f (length a) (length b))
                                    then longestStringHelper f (a:cs)
                                    else longestStringHelper f (b:cs)

-- longestString3
-- Consume a list of strings
-- Produce the longest string in the list, if there is a tie, the first one is returned
longestString3 :: [String] -> String
longestString3 xs = longestStringHelper (>=) xs

-- longestString4
-- Consume a list of strings
-- Produce the longest string in the list, if there is a tie, the last one is returned
longestString4 :: [String] -> String
longestString4 xs = longestStringHelper (>) xs

-- longestLowercase
-- Consume a list of strings
-- Produce the longest string that begins with a lowercase letter
longestLowercase :: [String] -> String
longestLowercase xs = (longestString.onlyLowercase) xs

-- revStringRev
-- Consume a string
-- Produce the string that is the same sequence of characters in reverse order
--  and every upper-case letter converted to a lower-case one
revStringRev :: String -> String
revStringRev [] = []
revStringRev (c:cs) = if (isLower c)
                          then revStringRev(cs) ++ [c]
                          else revStringRev(cs) ++ [toLower c]

-- firstAnswer
-- Consume a function and a list
-- Produce Just v for some v, or Nothing
firstAnswer :: (a -> Maybe b) -> [a] -> Maybe b
firstAnswer f [] = Nothing
firstAnswer f (x:xs) = case (f x) of
                          Nothing -> firstAnswer f xs
                          Just a -> Just a

-- allAnswers
-- Consume a function and a list
-- Produce a list of Just v for some v, or Nothing
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers f [] = Just []
allAnswers f xs = allAnswersHelper f [] xs

-- allAnswersHelper
-- Consume a function, accumulator, and list of elements left to do
-- Produces a list or Nothing
allAnswersHelper :: (a -> Maybe [b]) -> [b] -> [a] -> Maybe [b]
allAnswersHelper f acc [] = Just acc
allAnswersHelper f acc (x:xs) = case (f x) of
                                    Nothing -> Nothing
                                    Just a -> allAnswersHelper f (acc ++ a) xs

-- Pattern matching
-- Data definitions
data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern]) deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)

-- checkPat
-- Consume a Pattern
-- Produce a boolean, true if and only if all the variables appearing in the pattern
--  are distinct from each other
checkPat :: Pattern -> Bool
checkPat p = checkRepeats (listVariables p)

-- listVariables
-- Consume a Pattern
-- Produce a list of all the strings the Pattern uses for variables
listVariables :: Pattern -> [String]
listVariables WildcardPat = []
listVariables (VariablePat x) = [x]
listVariables UnitPat = []
listVariables (ConstantPat x) = []
listVariables (ConstructorPat (_, p)) = (listVariables p)
listVariables (TuplePat (p:ps)) = foldl (\f p -> f ++ listVariables p) [] ps

-- checkRepeats
-- Consume a list of Strings
-- Produce a boolean, True if all elements are unique, False if there is a repeat
checkRepeats :: [String] -> Bool
checkRepeats [] = True
checkRepeats (x:xs) = if elem x xs
                          then False
                          else checkRepeats xs

-- match
-- Consume a Value and a Pattern
-- Produce Nothing if the Pattern does not match
--  and Just lst where lst is the list of bindings if it does
match :: (Value, Pattern) -> Maybe [(String, Value)]
match (v, p) =
  case (v, p) of
    (_, WildcardPat) -> Just []
    (_, (VariablePat s)) -> Just [(s, v)]
    (Unit, UnitPat) -> Just []
    ((Constant x), (ConstantPat y)) -> if x == y then Just [] else Nothing
    ((Constructor(s2, v)),(ConstructorPat (s1, p))) -> if (s1 == s2) then match(v,p) else Nothing
    ((Tuple vs), (TuplePat ps)) -> if length vs == length ps
                                      then allAnswers (\t -> match (fst t, snd t)) (zip vs ps)
                                      else Nothing
    (_,_) -> Nothing

-- firstMatch
-- Consume a Value and a list of Patterns
-- Produce Nothing if no pattern in the list matches, or Just lst,
--  where lst is the list of bindings for the first pattern in the list that matches
firstMatch :: Value -> [Pattern] -> Maybe [(String, Value)]
firstMatch v [] = Nothing
firstMatch v ps = firstAnswer (\p -> match (v,p)) ps
