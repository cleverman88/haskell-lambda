{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR COURSEWORK 1 for COMP2209, 2019
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2019

module Exercises (histogram,approxSqrt,longestCommonSubsequence,neighbours,findBonding,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..),Instruction(..),Stack,SMProg,evalInst,findMaxReducers,optimalPower) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List
import Data.Function

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)


-- Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram _ [] = []
histogram n xs | n <= 0 = error "REEEEEEE"
               | otherwise = helper n xs 1
			   where 
			        helper _ [] _ = []
			        helper n xs y = [length[x | x <- xs, x<(n*y)]] ++ (helper (n) ([x | x <- xs, x>= (n*y)]) (y+1))

-- Exercise A2
approxSqrt :: Double -> Double -> Double

approxSqrt a epsilon | a < 0 = error "REEE"
                     | epsilon <= 0 = error "REEE"
                     | otherwise = last(helper [1.0] a epsilon)
     where 
  helper xs a epsilon
         | abs(	(last xs) - sqrt a) < epsilon = xs
         | otherwise = helper (xs ++ [((((last xs) + (a/(last xs))))/2)]) a epsilon

-- Exercise A3
longestCommonSubsequence2 :: Eq a => [a] -> [a] -> [a]
longestCommonSubsequence2 xs ys |  length xs == 0 || length ys == 0 = []
                               |  last xs == last ys = [last xs] ++ longestCommonSubsequence2 (reverse (tail (reverse xs))) (reverse (tail (reverse ys))) 
                               |  length x > length y = x
							   |  otherwise = y
							   where
							   x = longestCommonSubsequence2 xs (reverse (tail (reverse ys)))
							   y = longestCommonSubsequence2 (reverse (tail (reverse xs))) ys
							   
longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence [] = []
longestCommonSubsequence xss = helper (tail xss) (head xss)
	where
	helper :: Eq a => [[a]] -> [a] -> [a]
	helper xss a | length xss > 0 = helper (tail(xss)) (reverse (longestCommonSubsequence2 (head xss) a))
				 | otherwise = a

-- Exercise A4
type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double


neighbours ::  Int -> Metric a -> Point a -> [Point a] -> [Point a]

neighbours k d p xs | k < 0 = error "REEEEE"
					| otherwise = [y | (x,y) <- (take k (sortBy (\(a, _) -> \(b, _) -> compare a b ) [((d p x),x) | x <- xs]))]

-- Exercise A5
getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs (k:v:t) = (k,v) : getPairs t 

doubleUp xs = xs ++ [(y,x) | (x,y) <- xs]

solution xs p = sortBy (\(a,bs) -> \(c,ds) -> compare (length bs) (length ds))[(x, [y | y <- xs, x /= y, p x y, p y x]) | x <- xs]


getBondings [] _ = []
getBondings xs p | length (snd x) == 0 = []
                   | otherwise = (fst x, head(snd x)) :(head(snd x), fst x) : getBondings (delete (fst x) (delete (head(snd x)) xs)) p 
				   where 
				   x = head $ solution xs p
                   
				   
				   
removeFromLists xs x a = sortBy (\(a,bs) -> \(c,ds) -> compare (length bs) (length ds)) [(z,(delete a (delete x ys))) | (z,ys) <- xs, z /= x, z /= a]

findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe[(a,a)]
findBonding _ [] = Just []
findBonding p xs | (length xs) `mod` 2 == 1 = Nothing
				 | length x /= length xs = Nothing
				 | otherwise = Just x
				 where 
				 x = getBondings xs p

-- Exercise A6

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a

insertFromCurrentNode v z = inserting v (findPos v z)
									   
findPos a (Leaf, trail) = (Leaf,trail)
findPos a (node, [])    = (node,[])
	
findPos adding (Node cL cV cC cR,(L pV pC pN):t)   | (adding > cV && adding < pV) = returnParent (Node cL cV (cC) cR, ((L pV (pC) pN):t))
                                                   | adding == cV = (Node cL cV cC cR,(L pV pC pN):t)
												   | otherwise = findPos adding (returnParent (Node cL cV (cC) cR, ((L pV (pC) pN):t)))
												   
												   
findPos adding (Node cL cV cC cR,(R pV pC pN):t) |  (adding > cV && adding < pV) = returnParent (Node cL cV (cC) cR, ((R pV (pC) pN):t))
                                                   | adding == cV = (Node cL cV cC cR, (R pV pC pN):t)
												   | otherwise = findPos adding (returnParent (Node cL cV (cC) cR, ((R pV (pC) pN):t)))


returnParent (n, []) = (n,[])
returnParent (n, (R pV pC pL:t)) = ((Node pL pV (pC+1) n),t)
returnParent (n, (L pV pC pR:t)) = ((Node n pV (pC+1) pR),t)
 
 
 
inserting v (Leaf,t) = ((Node Leaf v 1 Leaf, t))

inserting v (Node l v2 c r,t) | v > v2  = inserting v (insertRight (Node l v2 c r,t)) 
                              | v == v2 = ((Node l v c r),t)
                              | otherwise  = inserting v (insertLeft (Node l v2 c r,t))

							 
                             
insertRight (Leaf,t) = (Leaf, t)
insertRight (Node l v c (Node a b c2 d),t) = ((Node a b (c2+1) d),(R v c l:t))
insertRight (Node l v c r,t) = (r,(R v c l:t))
                            
insertLeft (Leaf,t) = (Leaf, t)
insertLeft (Node (Node a b c2 d) v c r,t) = ((Node a b (c2+1) d),(L v c r:t))
insertLeft (Node l v c r,t) = (l,(L v c r:t))
							 
							 
							 
mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])

-- Exercise A7
evalInst :: Stack -> SMProg -> Stack

evalInst stack [] = stack 

evalInst stack bruh = solve2 stack (head bruh) bruh

solve2 stack (Add) prog | length stack < 2 = error "REEEEEEEEE"
                       | otherwise = evalInst ((head stack) + (head (tail stack)) : (tail (tail stack))) (tail prog) 
					   
solve2 stack (Mul) prog | length stack < 2 = error "REEEEEEEEE"
                       | otherwise = evalInst ((head stack) * (head (tail stack)) : (tail (tail stack))) (tail prog)
					   
solve2 stack (Dup) prog = evalInst ((head stack) : stack) (tail prog)

solve2 stack (Pop) prog | length stack == 0 = error "REEEEEEEEE"
                       | otherwise = evalInst (tail stack) (tail prog)

-- Exercise A8



evalInst2 :: Stack -> SMProg -> Stack

evalInst2 stack [] = stack 

evalInst2 stack bruh = solve stack (head bruh) bruh

solve stack (Add) prog | length stack < 2 = [-1]
                       | otherwise = evalInst2 ((head stack) + (head (tail stack)) : (tail (tail stack))) (tail prog) 
					   
solve stack (Mul) prog | length stack < 2 = [-1]
                       | otherwise = evalInst2 ((head stack) * (head (tail stack)) : (tail (tail stack))) (tail prog)
					   
solve stack (Dup) prog = evalInst2 ((head stack) : stack) (tail prog)

solve stack (Pop) prog | length stack == 0 = [-1]
                       | otherwise = evalInst2 (tail stack) (tail prog)
					   
generateSols 0 list = [list]
generateSols n list = generateSols (n-1) (Add : list) ++ (generateSols (n-1) (Mul : list)) ++ (generateSols (n-1) (Pop : list)) 

								
					   
findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers ss | length ss == 1 = [[]]
                   | otherwise = [y | (x,y)<- xs, fst (head xs) == x]
				   where xs = sortBy (\(a,bs) -> \(c,ds) -> compare c a)[(head (evalInst2 ss x), x) | x<- (generateSols ((length ss)-1) [])]

-- Exercise A9

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
