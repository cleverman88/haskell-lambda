{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A9 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (optimalPower,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

-- Exercise A9

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
					   
					
optimalPower :: Int -> SMProg		
optimalPower 1 = []
optimalPower n | n <= 0 = error "REEEEEEEEE"
               | otherwise = head [bruh | bruh <- [y | y<-(concat [generateValidStack x | x <- [1..n] ])], calculatePower (bruh) [1] == n]

findValid n 0 = (fmap ([Dup]++)) (findValid (n-1) (1))
findValid 0 d = [replicate d Mul] 
findValid n d = (fmap ([Mul]++) (findValid (n) (d-1))) ++ (fmap ([Dup]++) (findValid (n-1) (d+1)))
generateValidStack n = findValid n 0
					 
calculatePower [] ls = head ls
calculatePower (Dup:xs) ls = calculatePower xs ([head ls]++ls)
calculatePower (Mul:xs) ls = calculatePower xs ([((head ls) + (head (tail ls)))] ++ (tail (tail ls)))

					 