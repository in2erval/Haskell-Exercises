

data Vector = Vec2 Float Float | Vec3 Float Float Float deriving (Read, Eq)
data Plane = Plane Vector Float deriving (Read, Eq)

instance Num Vector where 
    Vec2 a b + Vec2 c d = Vec2 (a + c) (b + d)
    Vec3 a b c + Vec3 d e f = Vec3 (a + d) (b + e) (c + f)
    Vec2 a b - Vec2 c d = Vec2 (a - c) (b - d)
    Vec3 a b c - Vec3 d e f = Vec3 (a - d) (b - e) (c - f)

instance Show Vector where
    show (Vec2 a b) = "{" ++ show a ++ ", " ++ show b ++ "}"
    show (Vec3 a b c) = "{" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ "}"

instance Show Plane where
   show (Plane (Vec3 a b c) p) = show a ++ "x + " ++ show b ++ "y + " ++ show c ++ "z = " ++ show p

    

dot (Vec2 a b) (Vec2 c d) = (a * c) + (b * d)
dot (Vec3 a b c) (Vec3 d e f) = (a * d) + (b * e) + (c * f)

cross (Vec3 a b c) (Vec3 d e f) = Vec3 ((b * f) - (c * e)) ((c * d) - (a * f)) ((a * e) - (b * d))

mag (Vec2 a b) = sqrt (a^2 + b^2)
mag (Vec3 a b c) = sqrt (a^2 + b^2 + c^2)

angle vecA vecB = acos((dot vecA vecB) / ((mag vecA) * (mag vecB)))

degree :: Float -> Float
degree r = r * 180 / pi

