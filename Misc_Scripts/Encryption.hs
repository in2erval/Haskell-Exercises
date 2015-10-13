import Data.Char


-- map' takes an int, and rotates the alphabet by mod 26 of the given int.
map' :: Int -> String
map' seed = drop (seed `mod` 26) ['A'..'Z'] ++ take (seed `mod` 26) ['A'..'Z'] ++ drop (seed `mod` 10) ['0'..'9'] ++ take (seed `mod` 10) ['0'..'9']

-- keygen uses map' and returns a tuple with the cipher combination.
keygen :: Int -> [(Char, Char)]
keygen seed = zip (['A'..'Z'] ++ ['0'..'9']) (map' seed)

-- lookUp returns the character that the input char corresponds with when using the key generated by the input int. rev_lookUp does the reverse.
lookUp :: Int -> Char -> Char
lookUp int char = head [snd ans | ans <- (keygen int), (fst ans) == char]
rev_lookUp :: Int -> Char -> Char
rev_lookUp int char = head [fst ans | ans <- (keygen int), (snd ans) == char]

-- normalise takes a string, makes all characters uppercase, and filters out non-alphanumeric characters.
normalise :: String -> String
normalise text = filter (isAlphaNum) $ map (toUpper) text

-- encrypt generates a key using an int and the keygen function, and maps the lookUp function to all characters in the normalised input string.
encrypt :: Int -> String -> String
encrypt int text = map (lookUp int) $ normalise text

-- decrypt takes the key seed and the encrypted string to decode the text.
decrypt :: Int -> String -> String
decrypt int text = map (rev_lookUp int) text

