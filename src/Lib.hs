{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Lib where

import System.Random

import qualified Data.List as List

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Debug.Trace

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
elementAt (_:xs) index = elementAt xs (index - 1)

-- | Problem 4: Find the number of elements of a list.
myLength :: Integral d => [a] -> d
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

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
pack (x:x':xs)
  | x == head group = ([x] ++ group):groups
  | otherwise = [x]:group:groups
  where
    group:groups = pack (x':xs)

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
dropEvery :: Integral d => [a] -> d -> [a]
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
    helper (xs, []) _ = (xs, [])
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
    combination _ [] = []
    combination n (x:xs) = [(x:ys, zs) | (ys, zs) <- combination (n - 1) xs] ++ [(ys, x:zs) | (ys, zs) <- combination n xs]

-- | Problem 28 (a): Sorting a list of lists according to length of sublists
lsort :: [[a]] -> [[a]]
lsort = List.sortOn length

-- | Problem 28 (b): Sorting a list of lists according to length of frequency sublists
lfsort :: [[a]] -> [[a]]
lfsort xs = List.sortOn (\x -> frequencies ! (length x)) xs
  where
    frequencies :: Map Int Int
    frequencies = foldr (\x -> Map.insertWith (+) (length x) 1) Map.empty xs

-- | Problem 31: Determine whether a given integer number is prime.
isPrime :: Integral d => d -> Bool
isPrime n = List.find (>= n) primes == Just n

primes :: Integral d => [d]
primes = sieve_of_eratosthenes [2..]

sieve_of_eratosthenes :: Integral d => [d] -> [d]
sieve_of_eratosthenes [] = undefined
sieve_of_eratosthenes (x:xs) = x:sieve_of_eratosthenes [i | i <- xs, i `mod` x > 0]

-- | Problem 32: Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGCD :: Integral d => d -> d -> d
myGCD a b
  | b == 0 = abs a
  | otherwise = myGCD b (a `mod` b)

-- | Problem 33: Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integral d => d -> d -> Bool
coprime a b = gcd a b == 1

-- | Problem 34: Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
totient :: Integral d => d -> d
totient m = List.genericLength $ [r | r <- [1..(m - 1)], coprime r m]

-- | Problem 35: Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
primeFactors :: Integral d => d -> [d]
primeFactors n = concat [List.genericReplicate (multiplicity n p) p | p <- takeWhile (<= n) primes, n `mod` p == 0]

sqrt' :: Integral d => d -> d
sqrt' n = floor $ sqrt $ fromIntegral n

multiplicity :: Integral d => d -> d -> d
multiplicity dividend divisor = case dividend `divMod` divisor of
  (quotient, 0) -> 1 + multiplicity quotient divisor
  _ -> 0 

-- | Problem 36: Determine the prime factors of a given positive integer.
prime_factors_mult :: Integral d => d -> [(d, d)]
prime_factors_mult n = [(p, multiplicity n p) | p <- takeWhile (<= n) primes, n `mod` p == 0]

-- | Problem 37: Calculate Euler's totient function phi(m) (improved)
-- See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
-- 
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
--          (p2 - 1) * p2 ** (m2 - 1) * 
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- Note that a ** b stands for the b'th power of a.
totient' :: Integral d => d -> d
totient' m = product $ [(p - 1) * p ^ (k - 1) | (p, k) <- prime_factors_mult m]

-- | Problem 38: Compare the two methods of calculating Euler's totient function.
-- Use the solutions of problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.
-- (no solution required)
-- NOTE: gcd(a, b) has a time complexity of O(log(min(a, b)))
-- So coprime is also of O(log(min(a, b)))
-- totient from (34) is then of O(m^2 * log(m))
-- totient' from (37) is of O(m^2)

-- | Problem 39: A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
primesR :: Integral d => d -> d -> [d]
primesR lo hi = takeWhile (<= hi) $ dropWhile (<= lo) $ primes

-- | Problem 40: Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
goldbach :: Integral d => d -> (d, d)
goldbach n | n <= 2 || odd n = undefined
goldbach n = head [(x, y) | x <- possibilities, y <- filter (>= x) possibilities, x + y == n]
  where
    possibilities = takeWhile (<= n) primes

-- | Problem 41: Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
goldbachList :: Integral d => d -> d -> [(d, d)]
goldbachList lo hi = map goldbach $ takeWhile (<= hi) $ dropWhile (<= lo) evens
  where
    evens :: Integral d => [d]
    evens = [2, 4..]

goldbachList' :: Integral d => d -> d -> d -> [(d, d)]
goldbachList' lo hi limit = [(a, b) | (a, b) <- goldbachList lo hi, a > limit && b > limit]
