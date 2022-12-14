{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A7 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (evalInst,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

evalInst :: Stack -> SMProg -> Stack

evalInst stack [] = stack 

evalInst stack bruh = solve stack (head bruh) bruh

solve stack (Add) prog | length stack < 2 = error "REEEEEEEEE"
                       | otherwise = evalInst ((head stack) + (head (tail stack)) : (tail (tail stack))) (tail prog) 
					   
solve stack (Mul) prog | length stack < 2 = error "REEEEEEEEE"
                       | otherwise = evalInst ((head stack) * (head (tail stack)) : (tail (tail stack))) (tail prog)
					   
solve stack (Dup) prog = evalInst ((head stack) : stack) (tail prog)

solve stack (Pop) prog | length stack == 0 = error "REEEEEEEEE"
                       | otherwise = evalInst (tail stack) (tail prog)

data LamExpr = LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int deriving (Show, Eq)
                       
countAllReds :: LamExpr -> Int -> Int
countAllReds expr n
    | n==0 = 0
    | redexCount == 0 = 1 
    | otherwise =(sum $ map (\x -> (countAllReds x) $! (n-1)) reducedList)
        where 
            redexCount = countRedex expr
            reducedList = map (doNthReduction expr) [1..redexCount]
-- Counts the number of reducible expressions (redexes) in the given lambda expression
countRedex :: LamExpr -> Int
countRedex (LamApp (LamAbs x m) n) = 1 + countRedex m + countRedex n 
countRedex (LamApp m n) = countRedex m + countRedex n 
countRedex (LamAbs x m) = countRedex m
countRedex (LamVar x) = 0

-- Performs N-th reduction counting from the left
doNthReduction :: LamExpr -> Int -> LamExpr
doNthReduction (LamApp (LamAbs x m) n) index 
            | index == 1 = subst m x n
doNthReduction (LamApp m n) index
            | index == 0 = error "Can't call with argument 0"
            | index <= countRedex m =  (LamApp (doNthReduction m index) n)
doNthReduction (LamApp (LamAbs x m) n) index = (LamApp (LamAbs x m) (doNthReduction n (index - (countRedex m) - 1)))
doNthReduction (LamApp m n) index = (LamApp m (doNthReduction n (index - countRedex m)))
doNthReduction (LamAbs m n) index = (LamAbs m (doNthReduction n index))
doNthReduction (LamVar x) index = LamVar x

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = (LamVar x)
subst (LamAbs x e1) y e |
    x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e |
    x /= y && (free x e) = let x' = (fresh (LamAbs x e1)) in
        subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x==y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

fresh e  = (maximum $ getVars e) + 1 

getVars (LamVar x) = [x]
getVars (LamAbs x e) = x : (getVars (e))
getVars (LamApp f g) = (getVars f) ++ (getVars g)


free :: Int -> LamExpr -> Bool
free x (LamVar y) = x==y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x/= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)
