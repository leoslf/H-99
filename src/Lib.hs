{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import System.Random
import Control.Monad (replicateM)

import Data.List (sortOn)

import Data.Ord (comparing)
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- | Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- | Problem 2: Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (_:[]) = error "list with one element"
myButLast (x:_:[])  = x
myButLast (_:xs) = myButLast xs

-- | Problem 3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: Integral d => [a] -> d -> a
elementAt [] _ = error "empty list"
elementAt (x:_) 1 = x
elementAt (x:xs) index = elementAt xs (index - 1)

-- | Problem 4: Find the number of elements of a list.
myLength :: Integral d => [a] -> d
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- | Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- | Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs 

-- | Problem 7: Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs
  

-- | Problem 8: Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:x':xs)
  | x == x' = compress $ x:xs
  | otherwise = x:(compress $ x':xs)
-- compress = map head . group

-- | Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
  | x == head group = ([x] ++ group):groups
  | otherwise = [x]:group:groups
  where
    group:groups = pack xs

-- | Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\group -> (length group, head group)) $ pack xs

-- | Problem 11: Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data Eq a => ModifiedEncoding a
  = Single a
  | Multiple Int a
  deriving (Show, Eq)

modifiedEncoding :: Eq a => (Int, a) -> ModifiedEncoding a
modifiedEncoding (1, x) = Single x
modifiedEncoding (n, x) = Multiple n x

encodeModified :: Eq a => [a] -> [ModifiedEncoding a]
encodeModified xs = map modifiedEncoding $ encode xs

-- | Problem 12: Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: Eq a => [ModifiedEncoding a] -> [a]
decodeModified encoded = concat $ map decode encoded
  where
    decode (Single x) = [x]
    decode (Multiple n x) = replicate n x

-- | Problem 13: Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
encodeDirect :: Eq a => [a] -> [ModifiedEncoding a]
encodeDirect = map modifiedEncoding . encode'
  where
    encode' = foldr f []
    f x [] = [(1, x)]
    f x ((n, y):ys)
      | x == y = (n + 1, x):ys
      | otherwise = (1, x):(n, y):ys 

-- | Problem 14: Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- | Problem 15: Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ (repli xs n)

-- | Problem 16: Drop every N'th element from a lis.
dropEvery xs n = unenumerate $ filter (\(i, _) -> i `mod` n /= (n - 1)) $ enumerate xs
  where
    enumerate = zip [0..]
    unenumerate = map snd

-- | Problem 17: Split a list into two parts; the length of the first part is given.

-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split xs n = helper ([], xs) n
  where
    helper (xs, ys) 0 = (xs, ys)
    helper (xs, []) n = (xs, [])
    helper (xs, y:ys) n = helper (xs ++ [y], ys) (n - 1)

-- | Problem 18: Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice xs lo hi = drop (lo - 1) $ take hi xs

-- | Problem 19: Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate xs n = drop n' xs ++ take n' xs
  where
    n' = (length xs + n) `mod` (length xs)

-- | Problem 20: Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = (xs !! (k - 1), take (k - 1) xs ++ drop k xs)

-- | Problem 21: Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs index = take (index - 1) xs ++ [x] ++ drop (index - 1) xs

-- | Problem 22: Create a list containing all integers within a given range.
range :: Enum a => a -> a -> [a]
range lo hi = [lo..hi]

-- | Problem 23: Extract a given number of randomly selected elements from a list.
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n
  | n < 0 = error "n has to be > 0"
  | otherwise = do
      generator <- getStdGen
      return $ take n [xs !! index | index <- randomRs (0, length xs - 1)  generator]

-- | Problem 24: Lotto: Draw N different random numbers from the set 1..M.
diff_select :: Int -> Int -> IO [Int]
diff_select n m = rnd_select [1..m] n

-- | Problem 25: Generate a random permutation of the elements of a list.
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

-- | Problem 26: Generate the combinations of K distinct objects chosen from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
combinations :: Int -> [a] -> [[a]]
combinations k xs = backtrack xs [] (length xs) k 0
  where
    backtrack :: [a] -> [a] -> Int -> Int -> Int -> [[a]]
    backtrack xs current n k start
      | length current == k = [current]
      | otherwise = concat $ [backtrack xs (current ++ [xs !! i]) n k (i + 1) | i <- [start..(n - 1)]]

-- | Problem 27: Group the elements of a set into disjoint subsets.
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = [g:gs | (g, rs) <- combination n xs, gs <- group ns rs]
  where
    combination 0 xs = [([], xs)]
    combination n [] = []
    combination n (x:xs) = [(x:ys, zs) | (ys, zs) <- combination (n - 1) xs] ++ [(ys, x:zs) | (ys, zs) <- combination n xs]

-- | Problem 28 (a): Sorting a list of lists according to length of sublists
lsort :: [[a]] -> [[a]]
lsort = sortOn length

-- | Problem 28 (b): Sorting a list of lists according to length of frequency sublists
lfsort :: [[a]] -> [[a]]
lfsort xs = sortOn (\x -> frequencies ! (length x)) xs
  where
    frequencies :: Map Int Int
    frequencies = foldr (\x -> Map.insertWith (+) (length x) 1) Map.empty xs

