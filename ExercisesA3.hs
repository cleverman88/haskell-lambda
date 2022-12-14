--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A3 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (longestCommonSubsequence) where

-- Exercise A3


longestCommonSubsequence2 :: Eq a => [a] -> [a] -> [a]
longestCommonSubsequence2 xs ys |  length xs == 0 || length ys == 0 = []
                               |  last xs == last ys = [last xs] ++ longestCommonSubsequence2 (reverse (tail (reverse xs))) (reverse (tail (reverse ys))) 
                               |  length x > length y = x
							   |  otherwise = y
							   where
							   x = longestCommonSubsequence2 xs (reverse (tail (reverse ys)))
							   y = longestCommonSubsequence2 (reverse (tail (reverse xs))) y

							   
longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence [] = []
longestCommonSubsequence xss = helper (tail xss) (head xss)
	where
	helper :: Eq a => [[a]] -> [a] -> [a]
	helper xss a | length xss > 0 = helper (tail(xss)) (reverse (longestCommonSubsequence2 (head xss) a))
				 | otherwise = a