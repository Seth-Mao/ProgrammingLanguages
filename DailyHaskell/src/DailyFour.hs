module DailyFour where



--Question 1
--The function name is zip3Lists
--The type of function is zip3Lists :: [a] -> [b] -> [c] -> [(a, b, c)]
--Description of the parameters: polymorphic so that we can take different types of values such as strings chars ints etc. Three lists that turn into a list of tuples.
-- The resulting list of tuples will contain the the elements of the list consecutively

zip3Lists :: [a] -> [b] -> [c] -> [(a, b, c)]
--Base Cases to make sure not empty
zip3Lists [] _ _ = []
zip3Lists _ [] _ = []
zip3Lists _ _ [] = []
zip3Lists (head:tail) (head2:tail2) (head3:tail3) = (head, head2, head3) : zip3Lists tail tail2 tail3


--Question 2
--The function name is unZipTriples
--The type of function is unzipTriples :: [(a1, a2, a3)] -> ([a1], [a2], [a3])
--Description of the parameters: polymorphic so that we can take different types of values such as strings chars ints etc. 
-- The resulting tuple list will contain the first elements of the triples then second then third .... etc 

unzipTriples :: [(a1, a2, a3)] -> ([a1], [a2], [a3])
--Base Case to make sure not empty
unzipTriples [] = ([], [], [])
unzipTriples ((first, second, third):remainingTuples) = (first : firsts, second : seconds, third : thirds)
    where (firsts, seconds, thirds) = unzipTriples remainingTuples


--Question 3
--The function name is mergeSorted3
--The type of function is mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
--Description of the parameters: polymorphic so that we can take different types of values such as strings chars ints etc.  The function takes in three lists which are sorted and then merges them so that the list is in increasing order
-- The resulting list will contain all the values in ascending order. Followed the sorting method we learned in the prev class. MUST MAKE SURE THE LISTS ARE COMPARABLE

mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
--First List and second list must be sorted 
mergeSorted3 firstList secondList = mergeSorted (mergeSorted firstList secondList)
  where
    mergeSorted [] theRest = theRest
    mergeSorted theRest [] = theRest
    mergeSorted (x:remainingX) (y:remainingY)
    --Guards cause they cooler than if then statements (Prof said so)
    --MERGE TIME
        | x <= y    = x : mergeSorted remainingX (y:remainingY)
        | otherwise = y : mergeSorted (x:remainingX) remainingY
