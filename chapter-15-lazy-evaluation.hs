

-- Chapter 15: Lazy evaluation


-- 15.9.1
-- 1 + (2*3)
-- outermost: 1 + (2*3)
-- innermost: 2*3
-- 
-- (1+2) * (2+3) 
-- outermost: (1+2) * (2+3) 
-- innermost: (1+2)
-- 
-- fst (1+2, 2+3) 
-- outermost: fst (1+2, 2+3) 
-- innermost: 1+2
-- 
-- (\x -> 1+x) (2*3)
-- outermost: (\x -> 1+x) (2*3)
-- innermost: 2*3

-- 15.9.2
-- fst (1+2, 2+3) 
--
-- outermost evaluation:
-- fst (1+2, 2+3) 
-- -- fst --
-- 1+2
-- -- + --
-- 3
--
-- innermost evaluation:
-- fst (1+2, 2+3) 
-- -- + --
-- fst (3, 2+3) 
-- -- + --
-- fst (3, 5) 
-- -- fst --
-- 3
--
-- Outermost evaluations has two steps, innermost three. A step is saved.

-- 15.9.3
mult = \x -> (\y -> x*y)
-- mult 3 4
-- -- lambda --
-- (\x -> (\y -> x*y)) 3 4
-- -- apply outer lambda --
-- (\y -> 3*y) 4
-- -- apply outer lambda --
-- 3*4
-- -- * --
-- 12

-- 15.9.4
morefibs :: [Integer] -> [Integer]
morefibs (x:y:_) = x:(morefibs [y,z])
                   where z = x+y

fibs :: [Integer]
fibs = morefibs [0,1]

--nextfibs :: [Integer] -> [Integer]
--nextfibs xs = xs ++ [x+y | (x,y) <- zip xs (tail xs)]

-- 15.9.5
{-
repeat :: a -> [a]
repeat x = xs where xs = x:xs

take :: Int -> [a] -> [a]
take 0 _      = []
take n []     = []
take n (x:xs) = x : take (n-1) xs

replicate :: Int -> a -> [a]
replicate n = take n . repeat
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeat' :: a -> Tree a
repeat' x = Node (repeat' x) x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' 0 _            = Leaf
take' n Leaf         = Leaf 
take' n (Node l v r) = Node (take' smallhalf l) v (take' bighalf r)
                       where remainder = n-1
                             smallhalf = remainder `div` 2
                             bighalf = smallhalf + (remainder `mod` 2)

replicate' :: Int -> a -> Tree a 
replicate' n = take' n . repeat'

exampleTree :: Tree Int
exampleTree = (Node (Node Leaf 2 Leaf) 4 (Node Leaf 1 Leaf))
-- take' 1 exampleTree 
-- take' 2 exampleTree
-- take' 3 exampleTree
-- take' 4 exampleTree
-- replicate' 0 8
-- replicate' 1 8
-- replicate' 5 8

-- 15.9.6
next :: Double -> Double -> Double
next n = \a -> (a + n/a) / 2

approximate :: Double -> [Double]
approximate n = take 30 $ iterate (next n) 1.0

sqroot :: Double -> Double -> Double
sqroot n e = head (dropUntilSmallerThan e approximations)
             where approximations = iterate (next n) 1.0

dropUntilSmallerThan :: Double -> [Double] -> [Double]
dropUntilSmallerThan e (x:y:xs) = if abs (x-y) < e then [y] else dropUntilSmallerThan e (y:xs)

-- Using iterate with next means a new value is calculated in every iteration,
-- with all previous values also being kept. Not very similar to using strict application or foldl'.
