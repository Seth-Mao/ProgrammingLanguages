--Testing Push

module WeeklyOneHaskell where

--Function name removeChar
--Takes in a character and a string and then removes the character provided from that string
--From the book we can see that foldr allows us to go through a list of items (in this case a list of chars) and do something with them.
--IfEqual takes the current element and the result
--What we do with them is checking if each character in the string is equal to what chose then we DO NOT add it to the result if not then add

removeChar :: (Foldable t, Eq a) => a -> t a -> [a]
removeChar c = foldr ifEqual [] where
    ifEqual current result = 
        if current == c
            then result
            else current : result


--Function name removeWhiteSpace
--Takes in a string and then removes all spaces, tabs, newline characters, and carriage returns
--We use instances of removeChar and then compose them using the . operator for removeWhiteSpace

removeWhiteSpace :: String -> String
removeWhiteSpace = removeSpaces . removeTabs . removeNewlines . removeCarriageReturns

removeSpaces :: [Char] -> [Char]
removeSpaces = removeChar ' '

removeTabs :: [Char] -> [Char]
removeTabs = removeChar '\t'

removeNewlines :: [Char] -> [Char]
removeNewlines = removeChar '\n'

removeCarriageReturns :: [Char] -> [Char]
removeCarriageReturns = removeChar '\r'

--Function name removePunctuation
--Takes in a string and then removes all square brackets, curly brackets, parenthesis, commans, and periods.
--We use instances of removeChar and then compose them using the . operator for removePunctuation

removePunctuation :: String -> String
removePunctuation = removeCommas . removePeriods . removeParentheses . removeSquareBrackets . removeCurlyBrackets

removeCommas :: [Char] -> [Char]
removeCommas = removeChar ','

removePeriods :: [Char] -> [Char]
removePeriods = removeChar '.'

removeParentheses :: [Char] -> [Char]
removeParentheses = removeChar '(' . removeChar ')'

removeSquareBrackets :: [Char] -> [Char]
removeSquareBrackets = removeChar '[' . removeChar ']'

removeCurlyBrackets :: [Char] -> [Char]
removeCurlyBrackets = removeChar '{' . removeChar '}'


--Function name charsToAscii
--Takes in a string and then returns the list of integer representation of the chars in the string

charsToAscii :: String -> [Int]
charsToAscii [] = []
charsToAscii (head:tail) = fromEnum head : charsToAscii tail

--Function name asciiToChars
--Takes in a list of integers and then returns the list of the character representations

asciiToChars :: [Int] -> String
asciiToChars [] = []
asciiToChars (head:tail) = toEnum head : asciiToChars tail


--Function name shiftInts
--Takes in a shift amount and a list of strings then shifting the ints by the shift amount

shiftInts :: Integral a => a -> [a] -> [a]
shiftInts _ [] = []  
shiftInts shift (head:tail) = (head + shift) `mod` 128 : shiftInts shift tail


--Function name shiftMessage
--Takes in a integer to shift by and the message that wants to be encoded.
--This uses a really cool stategy that can only be achieved in haskell where we use the other three functions and compose the big function kinda how we would in OOP

shiftMessage :: Int -> String -> String
shiftMessage shift = asciiToChars . shiftInts shift . charsToAscii



