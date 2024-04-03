
module DailyOneSpec where

import Test.Hspec
import DailyOne

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "quadratic" $ do
    context "quadratic 0 0 0 1" $
      it "should be 0" $
        (quadratic 0 0 0 1) `shouldBe` 0

    context "quadratic 0 0 1 0" $
      it "should be 0" $
        (quadratic 0 0 1 0) `shouldBe` 0

    context "quadratic 0 1 0 0" $
      it "should be 0" $
        (quadratic 0 1 0 0) `shouldBe` 0

    context "quadratic 1 0 0 0" $
      it "should be 1" $
        (quadratic 1 0 0 0) `shouldBe` 1

   --NEW TEST 1     
    context "quadratic 1 2 3 4" $
      it "should be 57" $
        (quadratic 1 2 3 4) `shouldBe` 57        

   --NEW TEST 2     
    context "quadratic 0 0 0 0" $
      it "should be 0" $
        (quadratic 0 0 0 0) `shouldBe` 0

   --NEW TEST 3     
    context "quadratic 1 0 0 0" $
      it "should be -1" $
        (quadratic (-1) 0 0 0) `shouldBe` -1                        

  describe "scaleVector" $ do
    context "scaleVector 5 (1,0)" $
      it "should be (5, 0)" $
        (scaleVector 5 (1, 0)) `shouldBe` (5, 0)

    context "scaleVector 10 (0,1)" $
      it "should be (0, 10)" $
        (scaleVector 10 (0, 1)) `shouldBe` (0, 10)

    context "scaleVector 0 (1,1)" $
      it "should be (0, 0)" $
        (scaleVector 0 (0, 0)) `shouldBe` (0, 0)

    context "scaleVector 3 (2,3)" $
      it "should be (6, 9)" $
        (scaleVector 3 (2, 3)) `shouldBe` (6, 9)
  --NEW TEST 1
    context "scaleVector -2 (2,3)" $
      it "should be (-6, -9)" $
        (scaleVector 3 (2, 3)) `shouldBe` (6, 9)        

  --NEW TEST 2
    context "scaleVector 2 (10,-5)" $
      it "should be (20, -10)" $
        (scaleVector 2 (10, (-5))) `shouldBe` (20, (-10))        

  --NEW TEST 3
    context "scaleVector 1 (0,0)" $
      it "should be (0, 0)" $
        (scaleVector 1 (0, 0)) `shouldBe` (0, 0)        

  describe "tripleDistance" $ do
    context "tripleDistance (0,0,1) (0,0,0)" $
      it "should be 1.0" $
        (tripleDistance (0, 0, 1) (0, 0, 0)) `shouldBe` 1.0 

    context "tripleDistance (0,0,1) (0,0,-1)" $
      it "should be 2.0" $
        (tripleDistance (0, 0, 1) (0, 0, -1)) `shouldBe` 2.0 

    context "tripleDistance (0,0,1) (0,1,0)" $
      it "should be sqrt(2)" $
        (tripleDistance (0, 0, 1) (0, 1, 0)) `shouldBe` 
           (sqrt ((0 - 0)^2 + (0 - 1)^2 + (1 - 0)^2))

  --NEW TEST 1
    context "tripleDistance (1,2,3) (4,5,6)" $
      it " 5.196152422706632 " $
        (tripleDistance (1, 2, 3) (4, 5, 6)) `shouldBe` 
           (sqrt ((4 - 1)^2 + (5 - 2)^2 + (6 - 3)^2))

  --NEW TEST 2
    context "tripleDistance (0,0,0) (0,0,0)" $
      it "0" $
        (tripleDistance (0, 0, 0) (0, 0, 0)) `shouldBe` 
           (sqrt ((0 - 0)^2 + (0 - 0)^2 + (0 - 0)^2))

  --NEW TEST 3
    context "tripleDistance (-1,0,1) (0,1,0)" $
      it "1.7320508075688772" $
        (tripleDistance ((-1), 0, 1) (0, 1, 0)) `shouldBe` 
           (sqrt ((0 - (-1))^2 + (1- 0)^2 + (0 - 1)^2))