import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib

main :: IO ()
main = hspec $ do
  describe "myLast" $ do
    it "returns the last element of the list" $ do
      myLast [1, 2, 3, 4] `shouldBe` 4
      myLast ['x', 'y', 'z'] `shouldBe` 'z'
      evaluate (myLast []) `shouldThrow` errorCall "empty list"

  describe "myButLast" $ do
    it "returns the last but one element of a list" $ do
      myButLast [1, 2, 3, 4] `shouldBe` 3
      myButLast ['a'..'z'] `shouldBe` 'y'
      evaluate (myButLast []) `shouldThrow` errorCall "empty list"
      evaluate (myButLast [1]) `shouldThrow` errorCall "list with one element"

  describe "elementAt" $ do
    it "returns the K'th element of a list, 1-indexed" $ do
      elementAt [1,2,3] 2 `shouldBe` 2
      elementAt "haskell" 5 `shouldBe` 'e'

  describe "myLength" $ do
    it "returns the number of elements of a list" $ do
      myLength [123, 456, 789] `shouldBe` 3
      myLength "Hello, world!" `shouldBe` 13
      myLength [] `shouldBe` 0

  describe "myReverse" $ do
    it "returns the reversed copy of the list" $ do
      myReverse [] `shouldBe` ([] :: [Int])
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]

  describe "isPalindrome" $ do
    it "returns whether a list is a palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True

  describe "flatten" $ do
    it "flattens a nested list structure" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
      flatten (List []) `shouldBe` ([] :: [Int])

  describe "compress" $ do
    it "eliminates consecutive duplicates of list elements." $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
      compress [] `shouldBe` ([] :: [Int])

  describe "encode" $ do
    it "returns the run-length encoding of a list" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
