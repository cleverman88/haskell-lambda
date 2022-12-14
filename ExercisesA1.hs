--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A1 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (histogram) where

-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram _ [] = []
histogram n xs | n <= 0 = error "REEEEEEE"
               | otherwise = helper n xs 1
			   where 
			        helper _ [] _ = []
			        helper n xs y = [length[x | x <- xs, x<(n*y)]] ++ (helper (n) ([x | x <- xs, x>= (n*y)]) (y+1))
					
