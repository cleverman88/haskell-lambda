--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A2 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (approxSqrt) where

-- Exercise A2
approxSqrt :: Double -> Double -> Double

approxSqrt a epsilon | a < 0 = error "REEE"
                     | epsilon <= 0 = error "REEE"
                     | otherwise = last(helper [1.0] a epsilon)
     where 
  helper xs a epsilon
         | abs(	(last xs) - sqrt a) < epsilon = xs
         | otherwise = helper (xs ++ [((((last xs) + (a/(last xs))))/2)]) a epsilon
		 
		 
--approxSqrt :: Double -> Double -> Double

--approxSqrt a epsilon | a < 0 = error "REEE"
--                     | epsilon <= 0 = error "REEE"
--                     | otherwise = last(helper [1.0] a epsilon)
--     where 
--  helper xs temp temp2
--         | abs(last xs - (((last xs) + temp/(last xs))/2)) < temp2 = xs ++ [(((last xs) + temp/(last xs))/2)]
--         | otherwise = helper (xs ++ [(((last xs) + temp/(last xs))/2)]) temp temp2