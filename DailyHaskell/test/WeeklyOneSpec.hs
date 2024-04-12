{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} 
{-# HLINT ignore "Redundant bracket" #-}
module WeeklyOneSpec where

import Test.Hspec
import WeeklyOneHaskell

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "removeChar" $ do
        --Standard Test
        context "removeChar 'a' \"Haello\"" $
            it "should be \"Hello\"" $
                (removeChar 'a' ("Haello" :: [Char])) `shouldBe` "Hello"
        --Multiple Char test
        context "removeChar 'e' \"Blameeeeeeeee\"" $
            it "should be \"Blam\"" $
                (removeChar 'e' ("Blameeeeeee" :: [Char])) `shouldBe` "Blam"
        --Empty Test
        context "removeChar 'e' \"e\"" $
            it "should be \"\"" $
                (removeChar 'e' ("e" :: [Char])) `shouldBe` ""              
        --Standard Test
        context "removeWhiteSpace \"Hello \r \t\"" $
            it "should be \"Hello\"" $
                (removeWhiteSpace "Hello \r \t" ) `shouldBe` "Hello"
        --Empty Test
        context "removeWhiteSpace \"\r \t\"" $
            it "should be \"\"" $
                (removeWhiteSpace "\r \t" ) `shouldBe` ""
        --Standard Test
        context "removePunctuation \"What's Up Brother?!,.[]{}\"" $
            it "should be \"What's Up Brother?!\"" $
                (removePunctuation "What's Up Brother?!,.[]{}" ) `shouldBe` "What's Up Brother?!"
        --Standard Test
        context "charsToAscii \"a\"" $
            it "should be [97] " $
                (charsToAscii  "a")`shouldBe` [97]

        --Multiple Test
        context "charsToAscii \"\r \t\"" $
            it "should be [97,98,99,100,101,102,103] " $
                (charsToAscii  "abcdefg")`shouldBe` [97,98,99,100,101,102,103]

    --Invalid test
    --context "charsToAscii \"漢字\"" $
        --it "should be [28450,23383] " $
            --(charsToAscii "漢字") `shouldBe` [28450,23383]
        --Empty Test
        context "charsToAscii \"\"" $
            it "should be [] " $
                (charsToAscii  "") `shouldBe` []
        --Special Characters Test
        context "charsToAscii \"!@#$%^&*()\"" $
            it "should be [33,64,35,36,37,94,38,42,40,41] " $
                (charsToAscii "!@#$%^&*()") `shouldBe` [33,64,35,36,37,94,38,42,40,41]
        --Empty Test
        context "asciiToChars \"\"" $
            it "should be " $
                (asciiToChars  [] ) `shouldBe` ""
        --Standard Test
        context "asciiToChars [97]" $
            it "should be 'a'" $
                (asciiToChars [97]) `shouldBe` "a"  
        --Multiple Test
        context "asciiToChars [97,98,99,100,101,102,103]" $
            it "should be 'abcdefg'" $
                (asciiToChars [97,98,99,100,101,102,103]) `shouldBe` "abcdefg"
        
        --Standard Test
        context "shiftInts 1 [97,98,99,100,101,102,103]" $
            it "should be [98,99,100,101,102,103,104]" $
                (shiftInts 1 [97,98,99,100,101,102,103]) `shouldBe` [98,99,100,101,102,103,104]

        --Empty Test
        context "shiftInts 1 []" $
            it "should be []" $
                (shiftInts 1 []) `shouldBe` []

        --Revert Back Test
        context "shiftInts (-1) [97,98,99,100,101,102,103]" $
            it "should be [96,97,98,99,100,101,102]" $
                (shiftInts (-1) [97,98,99,100,101,102,103]) `shouldBe` [96,97,98,99,100,101,102]

        --Standard Test encryption
        context "shiftMessage 1 Hello" $
            it "should be Ifmmp" $
                (shiftMessage 1 "Hello") `shouldBe` "Ifmmp"

        --Standard Test Decryption
        context "shiftMessage (-1) Ifmmp" $
            it "should be Hello" $
                (shiftMessage (-1) "Ifmmp") `shouldBe` "Hello"
        --Empty Test
        context "shiftMessage 1 " $
            it "should be " $
                (shiftMessage 1 "") `shouldBe` ""
