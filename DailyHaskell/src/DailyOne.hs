module DailyOne where

--Question 1
--The function name is quadratic
--The type of function is quadratic :: Num a => a -> a -> a -> a -> a
--Description of the parameters: A,B,C,X are all integers that are then used to calculate the function quadratic. 

quadratic :: Num a => a -> a -> a -> a -> a
quadratic a b c x = a+(b*x) + (c*(x*x))

--Question 2
--The function name is scaleVector
--The type of function is scaleVector :: Num b => b -> (b, b) -> (b, b)
--Description of the parameters: The paramaters are one being a integer called multiple and a tuple (x,y) the multiple multiplies each of the values within the tuple

scaleVector :: Num b => b -> (b, b) -> (b, b)
scaleVector multiple (x,y) = (multiple *x, multiple * y )

--Question 3
--For the exponent I decided to multiple by itself since I was getting yellow lines when using (^2)
--The function name is tripleDistance
--The type of function is tripleDistance :: Floating a => (a, a, a) -> (a, a, a) -> a
--Description of the parameters: There are two parameters being two tuples with 3 variables being x,y,z. These variables represent the coordinates for points in a 3d plane and the equation finds the cartersian distance between them.

tripleDistance :: Floating a => (a, a, a) -> (a, a, a) -> a
tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt (((x2 - x1) *(x2 - x1) ) + ((y2 - y1) *(y2 - y1)) + ((z2 - z1) *(z2 - z1) ))