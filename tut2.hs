-- ex 1
import Debug.Trace
last2 :: [a] -> a
last2 [a] = a
last2 [] = error "Cant get last of an empty list"
last2 (x:xs) = last2 xs

last3 :: [a] -> a
last3 [] = error "Cant get last of an empty list"
last3 xs = head(drop (length (xs)-1) xs)

--ex 2
third1 :: [a] -> a
third1 xs = head(tail(tail(xs)))

third2 :: [a] -> a
third2 xs = xs!!2

-- Patten matching?
third3 :: [a] -> a
third3 [a] = [a]!!2


-- You get exceptions based on the functions you are using

--ex 3
safetail:: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail2:: [a] -> [a]
safetail2 xs | null xs = xs
             | otherwise = tail xs

safetail3:: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

--ex 4

halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs,drop (length xs `div` 2) xs)
-- Patten matching?

--ex 5
encrypt :: Int -> String -> (String, String -> String)
encrypt n ss = (enc, decrypt)
			   where enc = [toEnum(fromEnum(x)+n) | x <- ss]
			         decrypt ss = [toEnum(fromEnum(x) - n) | x <- ss]
					 
--ex 6
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (b + (luhnDouble a) + d +(luhnDouble c)) `mod` 10 == 0
               where luhnDouble x | x*2 > 9 = x*2-9
			                      | otherwise = x*2
								  
-- A1

histogram :: Int -> [Int] -> [Int]
histogram _ [] = []
histogram n xs = [length[x | x <- xs, x<n]] ++ histogram (2*n) ([x | x <- xs, x>= n])


-- A2

approxSqrt :: Double -> Double -> Double

approxSqrt a epsilon = last(helper [1.0] a epsilon)
     where 
  helper xs temp temp2
         | abs(last xs - (((last xs) + temp/(last xs))/2)) < temp2 = xs ++ [(((last xs) + temp/(last xs))/2)]
         | otherwise = helper (xs ++ [(((last xs) + temp/(last xs))/2)]) temp temp2

												 

			        
