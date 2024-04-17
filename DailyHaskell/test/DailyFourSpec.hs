module DailyFourSpec where

import Test.Hspec
import DailyFour


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "zip3Lists" $ do
     --Standard Test
     context " zip3Lists [1, 2, 3] ['a', 'b', 'c'][4, 5, 6]" $
        it "should be  [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]" $
            (zip3Lists [1, 2, 3] ['a', 'b', 'c'][4, 5, 6]) `shouldBe`  [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]

    --Empty Test
    context "zip3Lists [] [] []" $
        it "should be []" $
            (zip3Lists [] [] []) `shouldBe` ([] :: [((), (), ())])

    --All types test
    context "zip3Lists (zip3Lists [1, 2, 3] ['a', 'b', 'c'] [True, False, True]) " $
        it "should be [(1, 'a', True), (2, 'b', False), (3, 'c', True)]" $
            (zip3Lists [1, 2, 3] ['a', 'b', 'c'] [True, False, True]) `shouldBe` [(1, 'a', True), (2, 'b', False), (3, 'c', True)]
    
    describe "unzipTriples" $ do
    --Standard Test
        context " unzipTriples [ (1,2,3), (4, 5, 6), (7, 8, 9) ] " $
            it "should be  ( [1,4,7], [2, 5, 8], [3, 6, 9] )" $
                (unzipTriples [ (1,2,3), (4, 5, 6), (7, 8, 9) ]) `shouldBe`  ( [1,4,7], [2, 5, 8], [3, 6, 9] )

    --Empty Test
        context " unzipTriples [] " $
            it "should be  ( [], [], [] )" $
                (unzipTriples [ ]:: ([()], [()], [()])) `shouldBe`  ( [], [], [] )

    --All types test
    context "unzipTriples [(1, 'a', True), (2, 'b', False), (3, 'c', True)] " $
        it "should be ([1, 2, 3], \"abc\", [True, False, True]) " $
            (unzipTriples [(1, 'a', True), (2, 'b', False), (3, 'c', True)] ) `shouldBe` ([1, 2, 3], "abc", [True, False, True])


    describe "mergeSorted3" $ do
    --Standard Test
        context " mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10] " $
            it "should be  [-1, 0, 1, 2, 3, 4, 5, 8, 10]" $
                (mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10]) `shouldBe`  [-1, 0, 1, 2, 3, 4, 5, 8, 10]

    -- Empty Test
    context "mergeSorted3 with empty lists" $
        it "should be an empty list" $
            (mergeSorted3 [] [] []) `shouldBe` ([]:: [Int])  
    
    -- Different Type test
    context "mergeSorted3 ['a', 'a', 'a'] ['b', 'b', 'b'] ['c', 'c', 'c']" $
        it "should be ['a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c']" $
            (mergeSorted3 ['a', 'a', 'a'] ['b', 'b', 'b'] ['c', 'c', 'c']) `shouldBe` ['a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c']

