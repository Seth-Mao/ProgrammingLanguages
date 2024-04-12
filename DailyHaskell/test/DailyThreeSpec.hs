module DailyThreeSpec where 

import Test.Hspec

import DailyThree


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "removeAllExcept" $ do
     context "removeAllExcept 2 [1,2,3,4,5,6,7]" $
      it"should be [2]" $
      removeAllExcept 2 [1,2,3,4,5,6,7] `shouldBe` [2]

    describe "removeAllExcept" $ do
     context "removeAllExcept 'a' ['b', 'a', 'c', 'a'] " $
      it"should be aa" $
      removeAllExcept 'a' ['b', 'a', 'c', 'a']  `shouldBe` "aa"

    describe "removeAllExcept" $ do
     context "removeAllExcept  []" $
      it"should be " $
      removeAllExcept 'a' []  `shouldBe` []

    describe "countOccurences" $ do
     context "countOccurences 'a' ['a', 'b', 'a', 'c']" $
      it"should be 2 " $
      countOccurrences 'a' ['a', 'b', 'a', 'c'] `shouldBe` 2

    describe "countOccurences" $ do
     context "countOccurrences 1 [2, 4, 5, 2]" $
      it"should be 0 " $
      countOccurrences 1 [2, 4, 5, 2]  `shouldBe` 0

    describe "countOccurences" $ do
     context "countOccurrences 1 []" $
      it"should be 0 " $
      countOccurrences 1 []  `shouldBe` 0

    describe "substitute" $ do
     context " substitute 3 4 [1, 2, 3, 4]" $
      it"should be  [1, 2, 4, 4] " $
      substitute 3 4 [1, 2, 3, 4]  `shouldBe` [1, 2, 4, 4] 

    describe "substitute" $ do
     context " substitute 1 4 [1, 1, 1, 1]" $
      it"should be  [4, 4, 4, 4] " $
      substitute 1 4 [1, 1, 1, 1]  `shouldBe` [4, 4, 4, 4] 

    describe "substitute" $ do
     context " substitute 3 4 []" $
      it"should be  [] " $
      substitute 3 4 [] `shouldBe` [] 

