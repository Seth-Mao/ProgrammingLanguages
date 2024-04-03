module DailyOne where

--Question 1
--The function name is quadratic
--The type of function is quadratic :: Num a => a -> a -> a -> a -> a
--Description of the parameters: 

quadratic :: Num a => a -> a -> a -> a -> a
quadratic a b c x = a+(b*x) + (c*(x*x))

--Question 2
--The function name is quadratic
--The type of function is quadratic :: Num a => a -> a -> a -> a -> a
--Description of the parameters: 

scaleVector :: Num b => b -> (b, b) -> (b, b)
scaleVector multiple (x,y) = (multiple *x, multiple * y )

--Question 3
--For the exponent I decided to multiple by itself since I was getting errors when using (^2)
--The function name is quadratic
--The type of function is quadratic :: Num a => a -> a -> a -> a -> a
--Description of the parameters: 

tripleDistance :: Floating a => (a, a, a) -> (a, a, a) -> a
tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt (((x2 - x1) *(x2 - x1) ) + ((y2 - y1) *(y2 - y1)) + ((z2 - z1) *(z2 - z1) ))