-- qu1) double (2 + 2), (2+2 + 2+2)
double n = n + n
-- qu2) sum is defined as the sum x <- xs where xs is the list, therefore if xs only contains one element it will return the sum of the one element
-- sum [] = 0
-- sum (x:xs) = x + sum xs
-- qu3)
prod [] = 1
prod (x:xs) = x * product xs
-- qu4)
quicksort [] = []
quicksort(x:xs) = quicksort ls ++[x]++ quicksort rs
	where 
	ls = [a | a <- xs, a >= x]
	rs = [a | a <- xs, a < x]
-- qu5) It will only return the sorted list of UNIQUE numbers as the numbers that are the same will be skipped as they dont fall into the definition of either list
-- qu6) (2^3)*4
--	  (2*3)+(4*5)
--	   2+(3*(4^5))
--	  (2^2)+(2^2)
-- qu7)
n = a `div` length xs
	    where 
	    a = 10 
	    xs = [1,2,3,4,5]
-- qu8)
-- [Char]
-- (Char,Char,Char)
-- NONE
-- (Char, Integer, Bool)
-- NONE
-- [[a]->[a]]
-- [a]
-- NONE
-- [[a]]

-- qu9)
-- [True]
-- [[1],[2]]
-- add a b c d = a + b + c + d
-- copy a = (a,a)
-- apply a b = a b


-- qu10)
-- Num
-- (Num,Num)
-- (a -> b) - > (a,b)
-- Num a=> a-> a
-- a -> [a] -> Bool
-- (t -> t) -> t -> t
