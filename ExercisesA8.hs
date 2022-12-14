{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A8 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findMaxReducers,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Function


data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction]

instance NFData (Instruction)

-- Exercise A8


evalInst :: Stack -> SMProg -> Stack

evalInst stack [] = stack 

evalInst stack bruh = solve stack (head bruh) bruh

solve stack (Add) prog | length stack < 2 = [-1]
                       | otherwise = evalInst ((head stack) + (head (tail stack)) : (tail (tail stack))) (tail prog) 
					   
solve stack (Mul) prog | length stack < 2 = [-1]
                       | otherwise = evalInst ((head stack) * (head (tail stack)) : (tail (tail stack))) (tail prog)
					   
solve stack (Dup) prog = evalInst ((head stack) : stack) (tail prog)

solve stack (Pop) prog | length stack == 0 = [-1]
                       | otherwise = evalInst (tail stack) (tail prog)
					   
generateSols 0 list = [list]
generateSols n list = generateSols (n-1) (Add : list) ++ (generateSols (n-1) (Mul : list)) ++ (generateSols (n-1) (Pop : list)) 

								
					   
findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers ss | length ss == 1 = [[]]
                   | otherwise = [y | (x,y)<- xs, fst (head xs) == x]
				   where xs = sortBy (\(a,bs) -> \(c,ds) -> compare c a)[(head (evalInst ss x), x) | x<- (generateSols ((length ss)-1) [])]
				   
				