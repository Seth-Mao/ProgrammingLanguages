{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module DailyTwoSpec where 

import Test.Hspec
import DailyTwo


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
 describe "everyFourth" $ do
--Normal Behavior Test
    context "everyFourth [1..33]" $
     it"should be 4,8,12,16,20,24,28,32" $
     everyFourth [1..33] `shouldBe` [4,8,12,16,20,24,28,32]

--Less than four elements Behavior Test
    context "everyFourth [1,2,3]" $
     it"should be []" $
     (everyFourth [1,2,3]) `shouldBe` []

--Empty list Behavior Test
    context "everyFourth []" $
     it"should be []" $
     (everyFourth []) `shouldBe` ([] :: [Int]) 
 
--Normal Behavior Test
 describe "tupleDotProduct" $ do
    context "tupleDotProduct [2,2] [2,2]" $
     it"should be 8" $
     (tupleDotProduct [2,2] [2,2]) `shouldBe` 8

--Negative Number Behavior Test
 describe "tupleDotProduct" $ do
    context "tupleDotProduct [-2,2] [2,2]" $
     it"should be 0" $
     (tupleDotProduct [-2,2] [2,2]) `shouldBe` 0     

--Empty List Test
    context "tupleDotProduct [] []" $
        it "should be 0" $
            (tupleDotProduct [] []) `shouldBe` 0

--Zero Number Test            
    context "tupleDotProduct [1,0] [0,1]" $
        it "should be 0" $
            (tupleDotProduct [1,0] [0,1]) `shouldBe` 0     

 describe "appendToEach" $ do
--Normal Behavior Test
    context "appendToEach \"!!!\" [\"Hello\",\"GoodBye\"]" $
     it"should be [\"Hello!!!\",\"Goodbye!!!\"]" $
     (appendToEach "!!!" ["Hello","Goodbye"]) `shouldBe` ["Hello!!!","Goodbye!!!"]

--Other appends test
    context "appendToEach \"?\" [\"What's\",\"Up\"]" $
        it "should be [\"What's?\",\"Up?\"]" $
            (appendToEach "?" ["What's","Up","Brother"]) `shouldBe` ["What's?","Up?","Brother?"]

--Other appends test with space
    context "appendToEach \"?\" [\"What's\",\"Up\"]" $
        it "should be [\"dummy says what\",\"Bruh what\"]" $
            (appendToEach " what" ["dummy says","bruh"]) `shouldBe` ["dummy says what","bruh what"]

--Normal Behavior Test
 describe "toSetList" $ do
    context "toSetList [5,1,2,3,3,4,5,5]" $
     it"should be [1, 2, 3, 4, 5]" $
     (toSetList [5,1,2,3,3,4,5,5]) `shouldBe` [1, 2, 3, 4, 5]

--No duplicates Behavior Test
 describe "toSetList" $ do
    context "toSetList [1,2,3,4,5]" $
     it"should be [1, 2, 3, 4, 5]" $
     (toSetList [1,2,3,4,5]) `shouldBe` [1, 2, 3, 4, 5]
     