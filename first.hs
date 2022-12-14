type Pos = (Int,Int) 
func :: Pos -> Pos -> Int
func (a,b) (c,d) = a + b + c + d


reverse' [] ys = ys
reverse' (x:xs) ys = reverse1 xs ++ (x:ys)
reverse1 xs = reverse' xs [] 

