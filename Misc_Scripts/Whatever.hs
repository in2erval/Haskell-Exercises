intersperse' :: a -> [a] -> [a]
intersperse' _ [c] = [c]
intersperse' k (x:xs) = x : k : intersperse' k xs

max' :: (Ord a) => [a] -> a
max' = foldl1 (max)

quicksort [] = []
quicksort (x:xs) = (quicksort smaller) ++ [x] ++ (quicksort larger)
    where smaller = [a | a <- xs, a <= x]
          larger = [a | a <- xs, a > x]



test xs ys = [x | x <- xs, x `elem` ys]


