-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Go a)    = [Go a]
split (Turn a)  = [Turn a]
split Sit       = []
split (a :#: b) = split a ++ split b

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join c  = foldr1 (:#:) c

-- 1c  equivalent
equivalent c1 c2 = split c1 == split c2

-- 1d. testing join and split
prop_split_join c = equivalent c $ (join . split) c

prop_split c = and $ map f (split c)
    where f Sit = False
          f (_ :#: _) = False
          f x = True

-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy k = join . (replicate k)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon dist = copy 5 (Go dist :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon dist sides = copy sides (Go dist :#: Turn (360/(fromIntegral sides)))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral i 1 d a = Go i :#: Turn a
spiral i l d a = Go i :#: Turn a :#: (spiral (i + d) (abs (l - 1)) d a)


-- Exercise 4
-- optimise
--optimise :: Command -> Command
optimise = join . filt . recur . filt . split
filt = filter (`notElem` [Turn 0, Go 0, Sit])
recur []       = []
recur [a]      = [a]
recur (x:y:xs) = head (f x y):recur (tail ((f x y) ++ xs))
    where f (Turn a) (Turn b) = [Sit, Turn (a + b)]
          f (Go a) (Go b)     = [Sit, Go (a + b)]
          f a b               = [a, b]


com = Go 10 :#: Sit :#: Go 20 :#: Turn 35 :#: Go 0 :#: Turn 15 :#: Turn (-50)



a = let inDirection angle = Branch (Turn angle :#: Go 100) in
        join (map inDirection [20, 40..360])

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
    f 0 = GrabPen red :#: Go 10
    f x = g (x - 1) :#: (p :#: f (x - 1)) :#: (p :#: g (x - 1))
    g 0 = GrabPen blue :#: Go 10
    g x = f (x - 1) :#: (n :#: g (x - 1)) :#: (n :#: f (x - 1))
    n   = Turn 60
    p   = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
    where
    f 0 = GrabPen blue :#: Go 10
    f x = f (x - 1) :#: p :#: f (x - 1) :#: n :#: n :#: f (x - 1) :#: p :#: f (x - 1)
    n = Turn 60
    p = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x 
    where
    l 0 = GrabPen blue :#: Go 10
    l x = p :#: r (x - 1) :#: f :#: n :#: l (x - 1) :#: f :#: l (x - 1) :#: n :#: f :#: r (x - 1) :#: p 
    r 0 = GrabPen red :#: Go 10
    r x = n :#: l (x - 1) :#: f :#: p :#: r (x - 1) :#: f :#: r (x - 1) :#: p :#: f :#: l (x - 1) :#: n
    f = Go 30
    n = Turn 90
    p = Turn (-90)

