{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

-- | Problem 1. Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- | Problem 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (_:[]) = error "list with one element"
myButLast (x:_:[])  = x
myButLast (_:xs) = myButLast xs

-- | Problem 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: Integral d => [a] -> d -> a
elementAt [] _ = error "empty list"
elementAt (x:_) 1 = x
elementAt (x:xs) index = elementAt xs (index - 1)

-- | Problem 4. Find the number of elements of a list.
myLength :: Integral d => [a] -> d
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- | Problem 5. Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- | Problem 6. Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs 

-- | Problem 7. Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs
  

-- | Problem 8. Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:x':xs)
  | x == x' = compress $ x:xs
  | otherwise = x:(compress $ x':xs)
-- compress = map head . group

-- | Problem 9. Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
  | x == head group = ([x] ++ group):groups
  | otherwise = [x]:group:groups
  where
    group:groups = pack xs

-- | Probelm 10. Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\group -> (length group, head group)) $ pack xs
