-- Informatics 1 Functional Programming
-- December 2013
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f = undefined

-- 1b

g :: String -> Int
g = undefined


-- Question 2

-- 2a

p :: [Int] -> Bool
p = undefined

-- 2b

q :: [Int] -> Bool
q = undefined

-- 2c

r :: [Int] -> Bool
r = undefined

-- Question 3

data Expr = X
          | Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (Neg p)    =  "(-" ++ showExpr p ++ ")"
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- evaluate an Expr, given a value of X

evalExpr :: Expr -> Int -> Int
evalExpr X v          =  v
evalExpr (Const n) _  =  n
evalExpr (Neg p) v    =  - (evalExpr p v)
evalExpr (p :+: q) v  =  (evalExpr p v) + (evalExpr q v)
evalExpr (p :*: q) v  =  (evalExpr p v) * (evalExpr q v)

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM Neg subform
                                       , liftM2 (:+:) subform subform
                                       , liftM2 (:*:) subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- 3a

rpn :: Expr -> [String]
rpn (a :+: b) = (rpn a) ++ (rpn b) ++ ["+"]
rpn (a :*: b) = (rpn a) ++ (rpn b) ++ ["*"]
rpn (Neg a)   = (rpn a) ++ ["-"]
rpn (Const a) = [show a]
rpn X         = [show X]

-- 3 b

evalrpn :: [String] -> Int -> Int
evalrpn rpn val = head $ evalrpn' rpn val []

evalrpn' :: [String] -> Int -> [Int] -> [Int]
evalrpn' [] _ a = a
evalrpn' (x:xs) n intrm@(y:z:ys)
    | x == "+"  = evalrpn' xs n ((y + z):ys)
    | x == "*"  = evalrpn' xs n ((y * z):ys)
    | x == "-"  = evalrpn' xs n (-y:z:ys)
    | x == "X"  = evalrpn' xs n (n : intrm)
    | otherwise = evalrpn' xs n (read x : intrm)
evalrpn' (x:xs) n intrm@(y:ys)
    | x == "+"  = error "Not enough items in list"
    | x == "*"  = error "Not enough items in list"
    | x == "-"  = evalrpn' xs n (-y:ys)
    | x == "X"  = evalrpn' xs n (n : intrm)
    | otherwise = evalrpn' xs n (read x : intrm)
evalrpn' (x:xs) n intrm
    | x == "+"  = error "Not enough items in list"
    | x == "*"  = error "Not enough items in list"
    | x == "-"  = error "Not enough items in list"
    | x == "X"  = evalrpn' xs n (n : intrm)
    | otherwise = evalrpn' xs n (read x : intrm)

