import Data.List
import Data.Char

odds = sum . map (^2) . filter odd

odds' = filter (odd)

max' :: (Ord a) => [a] -> a
max' = foldl1 (max)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


split _ [] = []
split n xs = take n xs : split n (drop n xs)

concat' [] = []
concat' (x:xs) = x ++ concat xs

toDec :: [Char] -> [Int]
toDec xs = zipWith (*) (iterate (*16) 1) (reverse $ map digitToInt xs)

digitToInteger :: Char -> Integer
digitToInteger = undefined


mult' :: Integer -> Integer -> Integer
mult' a b = a * b

