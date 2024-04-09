module DailyTwo where

--Question 1
--The function name is everyFourth
--The type of function is everyFourth :: [a] -> [a]
--Description of the parameters: list is the parameter for the list provided that then is sent to the helper everyFourth prime that has three parameters. 
-- The three parameters are the list, index, and resulting list. The base case is checking if the list is empty return the empty result list otherwise check the index making sure that it is a multiple of 4 and then if it is add the value to result otherwise keep checking.
-- The type for the list is polymorphic as it can have different types such as char or integers
-- The resulting list will contain all the elements from indexes containing multiple of fours

everyFourth :: [a] -> [a]
everyFourth list = everyFourth' list 0 []
  where
    everyFourth' :: [a] -> Int -> [a] -> [a]
    everyFourth' [] _ result = result  
    everyFourth' (head:tail) index result =
      if index `mod` 4 == 3
        then everyFourth' tail (index + 1) (result ++ [head])  
        else everyFourth' tail (index + 1) result  

--Question 2
--The function name is tupleDotProduct
--The type of function is tupleDotProduct :: Num p => [p] -> [p] -> p
--Description of the parameters: two lists of same type specifically numbers
--The function result will have the dot product of the two lists
--The base case in this situation is when the lists are empty which would just result as zero as described in the HW notes.
--Dont have to check for same size since we assume they will always be the same size

tupleDotProduct :: Num p => [p] -> [p] -> p
tupleDotProduct [] [] = 0  
tupleDotProduct (q:p) (q2:p2) = q * q2 + tupleDotProduct p p2



--Question 3
--The function name is appendToEach
--The type of function is appendToEach :: [a] -> [[a]] -> [[a]]
--Description of the parameters: The function takes in a string and a list which then just appends the string to the list
--The function result will have the the string added to the end of each word in the list
--The base case in this situation is when the list is empty so it will just return an empty list

appendToEach :: [a] -> [[a]] -> [[a]]
appendToEach _ [] = []  
appendToEach appendString (head:tail) = (head ++ appendString) : appendToEach appendString tail



--Question 4
--The function name is toSetList
--The type of function is toSetList :: Eq a => [a] -> [a]
--Description of the parameters: A list
--The function result will have the list in set representation meaning that there are no duplicates
--The base case in this situation is when the lists are empty which would just result in sending back the empty list
-- The list is seperated into a head and tail the head being the first element while the tail is the remaning elements. 
-- It first checks if the head is part of the tail if it is then skip if not then add to the list and remove any duplicates caling back toSetList

toSetList :: Eq a => [a] -> [a]
toSetList [] = []  
toSetList (head:tail) =
--CHANGE ELEM TO HOMEMADE FUNCTION
    if head `elem` tail
        then toSetList tail 
        else head : toSetList [y | y <- tail, y /= head] 

