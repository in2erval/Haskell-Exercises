data Point = P Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (P x1 y1) (P x2 y2)) = (abs  $ x2 - x1) * (abs $ y2 - y1)

translate :: Shape -> Float -> Float -> Shape
translate (Circle (P x y) r) a b = Circle (P (x + a) (y + b)) r

baseCirc :: Float -> Shape
baseCirc a = Circle (P 0 0) a

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (P (0 - width / 2) (0 - height / 2)) (P (width / 2) (height / 2))