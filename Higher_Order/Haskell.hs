import Data.Char 
import Data.List

--1 (a)
uppers :: String -> String
uppers = map (toUpper)

--(b)
doubles :: [Int] -> [Int]
doubles = map (*2)

--(c)
--penceToPounds :: [Int] -> [Float]
penceToPounds = map (/ 100)


--2 (a)
alphas :: String -> String
alphas = filter (isAlpha)

--(b)
rmChar :: Char -> String -> String
rmChar char = filter (/= char)

--(c)
above :: Int -> [Int] -> [Int]
above num = filter (> num)

--(d)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (\(a,b) -> a /= b)


--3 (a)
func1 :: String -> String
func1 = map (toUpper) . filter (isAlpha)

--(b)
func2 :: [Int] -> [Int]
func2 = map (*2) . filter (>3)

--(c)
func3 :: [String] -> [String]
func3 = map (reverse) . filter (even . length)


--4 (a)
productRec :: [Int] -> Int
productRec [] = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

--(b)
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

--(c)
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs


concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

--(d)
rmCharsFold :: String -> String -> String
rmCharsFold strRem strIn = foldr (rmChar) strIn strRem