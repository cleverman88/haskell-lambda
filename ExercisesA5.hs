--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A5 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findBonding) where

-- Exercise A5

import Data.List
import Data.Function



getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs (k:v:t) = (k,v) : getPairs t 

doubleUp xs = xs ++ [(y,x) | (x,y) <- xs]

solve xs p = sortBy (\(a,bs) -> \(c,ds) -> compare (length bs) (length ds))[(x, [y | y <- xs, x /= y, p x y, p y x]) | x <- xs]


getBondings [] _ = []
getBondings xs p | length (snd x) == 0 = []
                   | otherwise = (fst x, head(snd x)) :(head(snd x), fst x) : getBondings (delete (fst x) (delete (head(snd x)) xs)) p 
				   where 
				   x = head $ solve xs p
                   
				   
				   
removeFromLists xs x a = sortBy (\(a,bs) -> \(c,ds) -> compare (length bs) (length ds)) [(z,(delete a (delete x ys))) | (z,ys) <- xs, z /= x, z /= a]

findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe[(a,a)]
findBonding _ [] = Just []
findBonding p xs | (length xs) `mod` 2 == 1 = Nothing
				 | length x /= length xs = Nothing
				 | otherwise = Just x
				 where 
				 x = getBondings xs p


				 
data LamExpr = LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int deriving (Show, Eq)

-- abstract data type for simple let expressions
data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)
-- END OF CODE YOU MUST NOT MODIFY


-- ADD YOUR OWN CODE HERE
-- Challenge 1
-- generate the alpha normal form for a simple lambda calculus expression
-- each bound variable is chosen to be the first one possible
alphaNorm :: LamExpr -> LamExpr
alphaNorm (LamVar x) = LamVar x 
alphaNorm (LamAbs e es) 
        | e < firstValid = (LamAbs e (alphaNorm es))
        | otherwise = (LamAbs firstValid (alphaNorm secondPart))
        where 
                secondPart = (subst es e (LamVar firstValid))
                firstValid = (head (dropWhile (\x -> (free x es)) [0..]))
alphaNorm (LamApp f g) = LamApp (alphaNorm f) (alphaNorm g)

-- Code taken from "Intepreters - Part I Substitutions" lecture by Dr Julian Rathke
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x==y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x/= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

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

-- Gets list of Ints of variable identifiers in given lambda expression
getVars (LamVar x) = [x]
getVars (LamAbs x e) = x : (getVars (e))
getVars (LamApp f g) = (getVars f) ++ (getVars g)

-- Helper function for finding a new variable identifier, when performing alpha conversion
fresh e  = (maximum $ getVars e) + 1 

