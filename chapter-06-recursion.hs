-- Chapter 6: Recursion

-- 6.8.1
factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)
            | otherwise = 1

-- 6.8.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 6.8.3
exp' :: Int -> Int -> Int
b `exp'` 0 = 1
b `exp'` e = b * (b `exp'` (e-1))

-- 6.8.4
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid (x-y) y
           | otherwise = euclid x (y-x)

-- 6.8.5
-- length [1,2,3]
-- 1 + (1 + (1 + 0))

-- drop 3 [1,2,3,4,5]
-- drop 2 [2,3,4,5]
-- drop 1 [3,4,5]
-- drop 0 [4,5]
-- [4,5]

-- init [1,2,3]
-- 1 : init [2, 3]
-- 1 : 2 : init [3]
-- 1 : 2 : []

-- 6.8.6.a
and''' :: [Bool] -> Bool
and''' [] = True
and''' (x:xs) | x == False = False
            | otherwise = and''' xs

-- 6.8.6.b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- 6.8.6.c
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

-- 6.8.6.d
atIndex :: [a] -> Int -> a
atIndex (x:xs) 0 = x
atIndex (x:xs) n = atIndex xs (n-1)

-- 6.8.6.e
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys

-- 6.8.7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 6.8.8
halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
           where half = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = x:[]
msort xs = merge (msort $ fst halves) (msort $ snd halves)
    where halves = halve xs
