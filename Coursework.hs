-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2019
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return a randome value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (alphaNorm, countAllReds, printLambda, parseLet, letToLambda,
    LamExpr(LamApp, LamAbs, LamVar), LetExpr(LetApp, LetDef, LetFun, LetVar, LetNum),
    lambdaToLet) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
--import Parsing

-- abstract data type for simple lambda calculus expressions
data LamExpr = LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int deriving (Show, Eq)

-- abstract data type for simple let expressions
data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)
-- END OF CODE YOU MUST NOT MODIFY

-- Notes
-- Define functions
-- Apply functions to them

-- Variables    x
-- Expressions  e <- Expression can be a variable (x) or it can be a an [abstraction (\x.e) Function with a parameter x and body e  <- Defining a function]
-- e1 e2  <- Application <- Applying a function to its argument


-- BETA REDUCTION (\x . e1) e2 => e1 [x |-> e2]
 
--(\x1 -> \x0) x0

-- ADD YOUR OWN CODE HERE
-- Challenge 1
-- generate the alpha normal form for a simple lambda calculus expression
-- each bound variable is chosen to be the first one possible
alphaNorm :: LamExpr -> LamExpr 


alphaNorm expr = finalRename (rename (getBound expr) []) []

finalRename (LamVar x) _ = LamVar x
finalRename (LamAbs x y) notAllowed  | x == -1 = LamAbs (getMax notAllowed) (finalRename y notAllowed)
                                     | otherwise = LamAbs x (finalRename y (x:notAllowed))
										  
finalRename (LamApp x y) notAllowed = LamApp (finalRename x ([c | c <- (getList x), c `elem` notAllowed ])) (finalRename y ([c | c <- (getList y), c `elem` notAllowed ]))


getMax xs | length xs == 0 = 0
		  | otherwise = (maximum $ xs) +1
                               




rename :: LamExpr -> [(Int,Int)] -> LamExpr

rename (LamVar x) xs | elem x [a | (a,y) <- xs] = LamVar (head [y | (a,y) <- xs, a==x])
                     | otherwise = (LamVar x)				
					 
rename (LamApp x y) xs = LamApp (rename x ([(d,e) | (d,e) <- xs, d `elem` [c | c <- (findAvailable x), c `elem` [a | (a,b) <- xs]]])) (rename y ([(d,e) | (d,e) <- xs, d `elem` [c | c <- (findAvailable y), c `elem` [a | (a,b) <- xs]]]))  


rename (LamAbs x y) xs | elem x [a | (a,b) <- xs] = (LamAbs (head [b | (a,b) <- xs, a==x]) (rename y xs))
                      | x /= (-1) = (LamAbs (findValid xs) (rename y ((x,(findValid xs)):xs) ))
					  | otherwise = (LamAbs x (rename y xs))

					  
getBound :: LamExpr -> LamExpr
				  
getBound (LamVar x) = LamVar x 
getBound (LamAbs x y) | x `elem` (getList y) = (LamAbs x (getBound y))
                      | otherwise =  (LamAbs (-1) (getBound y))
					  
getBound (LamApp x y) = LamApp (getBound x) (getBound y)

					   
					
-- Obtained from lecture slides
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x==y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x/= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

findValid xs | (length xs) == 0 = 0
             | otherwise = (maximum $ [y | (a,y) <- xs]) +1

findAvailable (LamAbs x y) = (findAvailable y)
findAvailable (LamApp x y) = (findAvailable x) ++ (findAvailable y)
findAvailable (LamVar x) = [x]


getList expr = [x | x <- (findAvailable expr),(free x expr)]

-- Challenge 2
-- count all reduction paths for a given lambda expression m, of length up to a given limit l
countAllReds :: LamExpr -> Int -> Int
countAllReds _ _ = -1


-- Challenge 3 
-- pretty print a lambda expression, combining abstraction variables
-- also recognising Scott numerals and printing these as numbers
-- finalising omitting brackets where possible and safe to do so
printLambda :: LamExpr -> String
printLambda xl = (solve xl)
 
-- \x -> \y -> x = 0
-- \x -> \y -> y 
-- \x1->\x1->x1 \x1->\x2->x1"
--solve (LamApp (LamVar a) b) = "x"++(show a) ++ printLambda b

solve (LamApp (LamVar a) b) = "x"++(show a) ++ printLambda b


solve (LamApp a b) = "("++ printLambda a ++ ")" ++ printLambda b
solve (LamAbs a b) = "\\x" ++ (show a) ++ "->" ++ printLambda b 
solve (LamVar a) = "x"++(show a)



-- Challenge 4
-- parse recursive let expression, possibly containing numerals
parseLet :: String -> Maybe LetExpr
parseLet _ = Just (LetVar (-1))


-- Challenge 5
-- translate a let expression into lambda calculus, using Scott numerals
-- convert let symbols to lambda variables using Jansen's techniques rather than Y
letToLambda :: LetExpr -> LamExpr
letToLambda _ = LamVar (-1)


-- Challenge 6
-- convert a lambda calculus expression into one using let expressions and application
-- can use lambda lifting techniques described in wikipedia article
lambdaToLet :: LamExpr -> LetExpr
lambdaToLet _ = LetVar (-1)

