-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = maximum((1 + depth left), (1 + depth right))

depth' Leaf = 0
depth' (Node _ _ left right) = 1 + (depth left `max` depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = toList left ++ (k, a):toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = f
    where
      f Leaf = Nothing
      f (Node k v left right) | key == k  = Just v
                              | key <= k  = f left
                              | otherwise = f right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList []           = Leaf
fromList ((x, y):lst) = set x y (fromList lst) 


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT key (Node k v left right)
    | k < key   = Node k v (filterLT key left) (filterLT key right)
    | otherwise = filterLT key left

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT key (Node k v left right)
    | k > key   = Node k v (filterGT key left) (filterGT key right)
    | otherwise = filterGT key right

-- Exercise 13

t1 = Node 10 "a" (Node 5 "n" (Node 2 "k" Leaf (Node 4 "j" Leaf Leaf)) (Node 6 "v" Leaf Leaf)) (Node 11 "q" Leaf Leaf)
t2 = Node 3 "a" (Node 1 "n" Leaf Leaf) (Node 7 "v" Leaf (Node 8 "o" Leaf (Node 9 "w" Leaf (Node 12 "l" Leaf Leaf))))

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge a Leaf = a
merge Leaf a = a
merge (Node k v left right) tree2 = Node k v (merge left (filterLT k tree2)) (merge right (filterGT k tree2))

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del _ Leaf = Leaf
del key (Node k v left right)
    | k == key  = merge left right
    | otherwise = Node k v (del key left) (del key right)

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select _ Leaf = Leaf
select pred (Node k v left right)
    | pred v    = Node k v (select pred left) (select pred right)
    | otherwise = merge (select pred left) (select pred right)

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary