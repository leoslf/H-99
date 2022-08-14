import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib

main :: IO ()
main = hspec $ do
  -- Problem 1
  describe "myLast" $ do
    it "returns the last element of the list" $ do
      myLast [1, 2, 3, 4] `shouldBe` 4
      myLast ['x', 'y', 'z'] `shouldBe` 'z'
      evaluate (myLast []) `shouldThrow` errorCall "empty list"

  -- Problem 2
  describe "myButLast" $ do
    it "returns the last but one element of a list" $ do
      myButLast [1, 2, 3, 4] `shouldBe` 3
      myButLast ['a'..'z'] `shouldBe` 'y'
      evaluate (myButLast []) `shouldThrow` errorCall "empty list"
      evaluate (myButLast [1]) `shouldThrow` errorCall "list with one element"

  -- Problem 3
  describe "elementAt" $ do
    it "returns the K'th element of a list, 1-indexed" $ do
      elementAt [1,2,3] 2 `shouldBe` 2
      elementAt "haskell" 5 `shouldBe` 'e'

  -- Problem 4
  describe "myLength" $ do
    it "returns the number of elements of a list" $ do
      myLength [123, 456, 789] `shouldBe` 3
      myLength "Hello, world!" `shouldBe` 13
      myLength [] `shouldBe` 0

  -- Problem 5
  describe "myReverse" $ do
    it "returns the reversed copy of the list" $ do
      myReverse [] `shouldBe` ([] :: [Int])
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]

  -- Problem 6
  describe "isPalindrome" $ do
    it "returns whether a list is a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True

  -- Problem 7
  describe "flatten" $ do
    it "flattens a nested list structure" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
      flatten (List []) `shouldBe` ([] :: [Int])

  -- Problem 8
  describe "compress" $ do
    it "eliminates consecutive duplicates of list elements." $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
      compress [] `shouldBe` ([] :: [Int])

  -- Problem 9
  describe "pack" $ do
    it "packs consecutive duplicates of list elements into sublists" $ do
      pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  -- Problem 10
  describe "encode" $ do
    it "returns the run-length encoding of a list" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

  -- Problem 11
  describe "encodeModified" $ do
    it "packages the encoding into custom datatype" $ do
      encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

  -- Problem 12
  describe "decodeModified" $ do
    it "decodes the custom datatype of the run-length encoding" $ do
      decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"

  -- Problem 13
  describe "encodeDirect" $ do
    it "run-length encodes a list directly" $ do
      encodeDirect "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

  -- Problem 14
  describe "dupli" $ do
    it "duplicate the elements of a list" $ do
      dupli [1, 2, 3] `shouldBe` [1,1,2,2,3,3]

  -- Problem 15
  describe "repli" $ do
    it "replicates the elements of a list a given number of times" $ do
       repli "abc" 3 `shouldBe` "aaabbbccc"

  -- Problem 16
  describe "dropEvery" $ do
    it "drops every n-th element from a list" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
  
  -- Problem 17
  describe "split" $ do
    it "splits a list into two parts; the length of the first part is given." $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  -- Problem 18
  describe "slice" $ do
    it "extracts a slice from a list" $ do
      slice "abcdefghik" 3 7 `shouldBe` "cdefg"

  -- Problem 19
  describe "rotate" $ do
    it "rotates a list n places to the left" $ do
      rotate "abcdefgh" 3 `shouldBe` "defghabc"
      rotate "abcdefgh" (-2) `shouldBe` "ghabcdef"

  -- Problem 20
  describe "removeAt" $ do
    it "removes the k-th element from a list" $ do
      removeAt 2 "abcd" `shouldBe` ('b',"acd")
