quicksort [] = []
quicksort(x:xs) = quicksort ls++ [x] ++ quicksort rs
    where ls = [a| a <- xs, a < x]
          rs = [a| a <- xs, a > x]
	