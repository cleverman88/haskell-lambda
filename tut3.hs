-- ex1
--sum([x^2| x <- [1..100], x `mod` 2 == 1] ++ [x^3 | x<- [1..100], x `mod` 2 == 0])

-- ex2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m],y <- [0..n]]
square :: Int  -> [(Int,Int)]
square m = [(x,y) | x <- [0..m] , y <- [0..m] , x /= y]

-- ex3
replicate2 :: Int -> a ->[a]
replicate2 0 _ = []
replicate2 repeat val = [x | (x,y) <- [(x,y) | y <- [0..repeat] ,x <- [val]]]

--ex 4
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z)|  x <- [0..n], y <- [0..n], z<- [max (y+1) (x+1)..n], (x^2) + (y^2) == z^2]

--ex 5
perfect :: Int -> [Int]
perfect limit = [x | x <- [1..limit], sum [y | y<- [1..x-1] ,x `mod` y == 0] == x ]

--ex 6
find :: Eq a => a -> [ (a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']

positions2 :: Eq a => a -> [a] -> [Int]

positions2 a xs = find a (zip xs [0..])

--ex 7
scalarProduct xs ys = sum [x*y| (x,y) <- zip xs ys]

--ex 8
euclid :: Int -> Int -> Int
euclid a b | b == 0 = a
           | otherwise = gcd b (a `mod` b)
		   
		   
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
longestCommonSubsequence xss = helper (tail xss) (head xss)
	where
	helper :: Eq a => [[a]] -> [a] -> [a]
	helper xss a | length xss > 0 = helper (tail(xss)) (reverse (longestCommonSubsequence2 (head xss) a))
				 | otherwise = a

							   