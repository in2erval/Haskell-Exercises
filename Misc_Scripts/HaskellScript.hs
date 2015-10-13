import Test.QuickCheck

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
   where smaller = [a | a <- xs, a <= x]
         larger = [a | a <- xs, a > x]

mult3 :: (Num a) => a -> a -> a -> a
mult3 x y z = x * y * z

twice func = func . func 

fibonacci n = fib' 1 1 (n-2)
 where fib' x y z = if z <= 0
                    then y
                    else fib' y (x+y) (z-1)

isUpperAlphanum = (`elem` ['A'..'Z'])

apply2x :: (a -> a) -> a -> a
apply2x f x = f (f x)

flip' f x y = f y x

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' _ [] = []
map' func (x:xs) = func x : map' func xs

filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs



chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

largestChain n = computeChain n 1
computeChain y z 
    | y == 0 = chain z
    | length (chain y) > length (chain z) = computeChain (y-1) y
    | otherwise = computeChain (y-1) z


sum' :: (Num a) => [a] -> a
sum' = foldl1 (+) 

elem' :: (Eq a) => a -> [a] -> Bool
elem' z = foldl (\x y -> if y==z then True else x) False




squaresRec [] = []
squaresRec (x:xs) = x*x : squaresRec xs

squaresRec' = map (^2)

listComp xs = [x+10 | x <- xs, x > 10]
