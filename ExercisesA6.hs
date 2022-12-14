{-# LANGUAGE DeriveGeneric #-}

--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A6 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq 

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)


-- Exercise A6

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a

insertFromCurrentNode v z = inserting v (findPos v z)
									   
findPos a (Leaf, trail) = (Leaf,trail)
findPos a (node, [])    = (node,[])
	
findPos adding (Node cL cV cC cR,(L pV pC pN):t) | adding == cV = (Node cL cV cC cR,(L pV pC pN):t)
                                                   | (adding > cV && adding < pV) = returnParent (Node cL cV (cC) cR, ((L pV (pC) pN):t))
												   | otherwise = findPos adding (returnParent (Node cL cV (cC) cR, ((L pV (pC) pN):t)))
												   
												   
findPos adding (Node cL cV cC cR,(R pV pC pN):t) | adding == cV = (Node cL cV cC cR, (R pV pC pN):t)
                                                   | (adding > cV && adding < pV) = returnParent (Node cL cV (cC) cR, ((R pV (pC) pN):t))
												   | otherwise = findPos adding (returnParent (Node cL cV (cC) cR, ((R pV (pC) pN):t)))


returnParent (n, []) = (n,[])
returnParent (n, (L pV pC pR:t)) = ((Node n pV (pC+1) pR),t)
returnParent (n, (R pV pC pL:t)) = ((Node pL pV (pC+1) n),t)
 
 
 
inserting v (Leaf,t) = ((Node Leaf v 1 Leaf, t))
inserting v (Node l v2 c r,t) | v == v2 = ((Node l v c r),t)
                              | v > v2  = inserting v (insertRight (Node l v2 c r,t))
                              | otherwise  = inserting v (insertLeft (Node l v2 c r,t))

							 
insertLeft (Leaf,t) = (Leaf, t)
insertLeft (Node (Node a b c2 d) v c r,t) = ((Node a b (c2+1) d),(L v c r:t))
insertLeft (Node l v c r,t) = (l,(L v c r:t))

insertRight (Leaf,t) = (Leaf, t)
insertRight (Node l v c (Node a b c2 d),t) = ((Node a b (c2+1) d),(R v c l:t))
insertRight (Node l v c r,t) = (r,(R v c l:t))
							 
							 
							 
mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])


