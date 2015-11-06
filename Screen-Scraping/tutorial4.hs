-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (22/23 Oct)

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString str1 str2 = a str1 == a str2
                       where a = map toUpper


-- 2.
prefix :: String -> String -> Bool
prefix pfix str = sameString pfix (take (length pfix) str) 

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
		         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
        
        
-- 3.
contains :: String -> String -> Bool
contains _ [] = False
contains str1 (str2:rest) = prefix str1 (str2:rest) || contains str1 rest

prop_contains :: String -> Int -> Int -> Bool
prop_contains = undefined

--"ll" "Hello"
-- 4.
takeUntil :: String -> String -> String
takeUntil _ [] = []
takeUntil cond (top:str)
    | take (length cond) (top:str) `sameString` cond = []
    | otherwise = top : takeUntil cond str

dropUntil :: String -> String -> String
dropUntil _ [] = []
dropUntil cond (top:str)
    | take x (top:str) `sameString` cond = drop x (top:str)
    | otherwise = dropUntil cond str
    where x = length cond


-- 5.
split :: String -> String -> [String]
split _ [] = []
split delimit str = takeUntil delimit str : split delimit (dropUntil delimit str)

reconstruct :: String -> [String] -> String
reconstruct _ [] = []
reconstruct splitter (str:rest) = str ++ splitter ++ reconstruct splitter rest

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML html = [a | a <- split "href=\"" html, not (contains "<html>" a)]

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails = filter (contains "mailto")


-- 8.
link2pair :: Link -> (Name, Email)
link2pair links = (takeUntil "<" (dropUntil ">" links), takeUntil "\"" (dropUntil "mailto:" links))


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML = undefined

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail = undefined


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML = undefined


-- Optional Material

-- 12.
--hasInitials :: String -> Name -> Bool
hasInitials initials name = or $ zipWith (prefix) (split' initials) (split " " name)
    where split' [] = []
          split' (a:str) = [a] : split' str

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML = undefined

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML = undefined

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria = undefined

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML = undefined

-- 15
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
