module DailyThree where

--Question 1
--The function name is removeAllExcept
--The type of function is removeAllExcept :: Eq a => a -> [a] -> [a]
--Description of the parameters: polymorphic so that we can take different types of values such as strings chars ints etc. A list and a value to compare within the list
-- The resulting list will contain only the values equal to the one provided

removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept _ [] = []
removeAllExcept target (head:tail) =
    if target == head
        --Create new list with head and recursively do it again to tail
        then head : removeAllExcept target tail
        --If not equal then continue
        else removeAllExcept target tail


--Question 2
--The function name is countOccurrences
--The type of function is countOccurrences :: Eq a => a -> [a] -> Int
--Description of the parameters: polymorphic so that we can take different types of values such as strings chars ints etc. A list and a value to count within the list
-- The result will provide an int with the count of the specified target

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences _ [] = 0
countOccurrences target (head:tail)=
    if target == head
        then 1 + countOccurrences target tail 
        else countOccurrences target tail

--Question 3
--The function name is substitute
--The type of function is substitute :: Eq a => a -> a -> [a] -> [a]
--Description of the parameters: polymorphic so that we can take different types of values such as strings chars ints etc. A list and a value to compare within the list and the one to replace
-- The resulting list will have the target value replaced with the substitute value

substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute target sub (head:tail) = 
        if head == target
            then sub : substitute target sub tail
            else  head: substitute target sub tail