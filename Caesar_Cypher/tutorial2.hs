-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 15/16 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char]
rotate seed = drop (seed `mod` 26) ['A'..'Z'] ++ take (seed `mod` 26) ['A'..'Z'] ++ drop (seed `mod` 10) ['0'..'9'] ++ take (seed `mod` 10) ['0'..'9']

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey seed = zip (['A'..'Z'] ++ ['0'..'9']) (rotate seed)

-- 4.
lookUp :: Int -> Char -> Char
lookUp int char = head [snd ans | ans <- (makeKey int), (fst ans) == char]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec = undefined

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp = undefined

-- 5.
encipher :: Int -> Char -> Char
encipher = undefined

-- 6.
normalise :: String -> String
normalise text = filter (isAlphaNum) $ map (toUpper) text

-- 7.
encipherStr :: Int -> String -> String
encipherStr int text = map (lookUp int) $ normalise text

-- 8.
reverseKey :: Int -> Char -> Char
reverseKey int char = head [fst ans | ans <- (makeKey int), (snd ans) == char]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec = undefined

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey = undefined
-- 9.
decipher :: Int -> Char -> Char
decipher = undefined

decipherStr :: Int -> String -> String
decipherStr = undefined

-- 10.
contains :: String -> String -> Bool
contains _ [] = False
contains str1 (str2:rest)
    | isPrefixOf str1 (str2:rest) = True
    | otherwise = contains str1 rest

-- 11.
candidates :: String -> [(Int, String)]
candidates = undefined



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive = undefined

-- 13.
prop_transpose :: String -> Bool
prop_transpose = undefined

-- 14.
encrypt :: Int -> String -> String
encrypt = undefined

-- 15.
decrypt :: Int -> String -> String
decrypt = undefined

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined
