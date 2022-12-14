import Data.List

-- ex2)
dec2Int :: [Int] -> Int
dec2Int [] = 0
--dec2Int xs = (10^((length xs)-1) * (head xs)) + (dec2Int (tail xs))
dec2Int xs = foldr (\(x,y) z-> (x * 10^y + z)) 0 (zip xs [((length xs)-1),((length xs)-2)..0])
--dec2Int xs = foldl (\x (a,b) -> (x + (a^(b))) 0 (zip xs [0..])

-- ex3)

-- curry2 :: ((a,b) -> c) -> a -> b -> c
-- uncurry2 :: a -> (b -> c)

--ex4)
unfold p h t x | p x       = []
                   | otherwise = h x : unfold p h t (t x)
			
int2bin :: Int -> [Int]
int2bin x = reverse (unfold (\x -> (x==0)) (\x -> (x `mod` 2)) (\x -> (quot x 2)) x)

--ex5)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g xs = f (head xs) : g (head(tail xs)) : altMap f g (tail (tail xs))

--ex7)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show
--toTree :: Ord => [a] -> Tree a

first_halve :: [a] -> [a]
first_halve = (\xs -> case xs of
            [] -> []
            xs -> take ((length xs) `div` 2) xs)

second_halve :: [a] -> [a]
second_halve = (\xs -> case xs of
            [] -> []
            xs -> drop ((length xs) `div` 2 + 1) xs)


toTree [] = Leaf		  
toTree xs = Node (toTree (first_halve xs)) (xs!! (length xs)/2) (toTree (second_halve xs))

--A4)

type Metric a = Point a -> Point a -> Double
type Point a = (a,a)
		  

neighbours k d p xs | k < 0 = error "REEEEE"
					| otherwise = [y | (x,y) <- (take k (sortBy (\(a, _) -> \(b, _) -> compare a b ) [((d p x),x) | x <- xs]))]





 
