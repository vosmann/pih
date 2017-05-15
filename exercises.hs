-- Chapter 4: Defining functions

-- 4.8.1
halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
    where half = length xs `div` 2

-- 4.8.2
-- a
third :: [a] -> a
third xs = head (tail (tail xs))
-- b
third' :: [a] -> a
third' xs = xs !! 2
-- c
third'' :: [a] -> a
third'' (_:_:z:_) = z

--4.8.3
-- a
safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

-- b
safetail' :: [a] -> [a]
safetail' xs | null xs = xs
             | otherwise = tail xs

-- c
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

-- 4.8.4
-- a
(|||) :: Bool -> Bool -> Bool
False ||| False = False
False ||| True = True
True ||| False = True
True ||| True = True

-- b
or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

-- c
or'' :: Bool -> Bool -> Bool
or'' False y = y
or'' True _ = True

-- d
or''' :: Bool -> Bool -> Bool
or''' x y | x == y = x 
          | otherwise = True

-- 4.8.5
and' :: Bool -> Bool -> Bool
and' x y = if x then (if y then True else False) else False

-- 4.8.6
and'' :: Bool -> Bool -> Bool
and'' x y = if x then y else False

-- 4.8.7
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

mult' :: Int -> Int -> Int -> Int
mult' = \x -> \y -> \z -> x*y*z

-- 4.8.8 -- Luhn
luhnDouble :: Int -> Int
luhnDouble x | doubled > 9 = doubled - 9
             | otherwise   = doubled
               where doubled = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | sum `mod` 10 == 0 = True
             | otherwise = False
             where sum = (luhnDouble a) + b + (luhnDouble c) + d


-- Chapter 5: List comprehensions

-- 5.7.1
sumSquaresTo n = sum [x*x | x <- [1..n]]

-- 5.7.2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 5.7.3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 5.7.4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 5.7.5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 5.7.6
factors :: Int -> [Int]
factors n = [x | x <- [1..(n `div` 2)], n `mod` x == 0 ]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == (sum $ factors x)]

-- 5.7.7
oneComprehensionTwoGenerators = [(x, y) | x <- [1, 2], y <- [3, 4]]
twoComprehensionsOneGenerator = [pair | pair <- concat [[(1, y1) | y1 <- [3, 4]], [(2, y2) | y2 <- [3, 4]]]]
twoComprehensionsOneGenerator' = concat [ [(x, y) | x <- [1, 2]] | y <- [3, 4] ]

-- 5.7.8
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v' | (k', v') <- t, k == k' ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x $ zip xs [0..]

-- 5.7.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]


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
replicate'' :: Int -> a -> [a]
replicate'' 0 x = []
replicate'' n x = x : replicate' (n-1) x

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
--halve :: [a] -> ([a], [a])
--halve xs = (take half xs, drop half xs)
--    where half = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = x:[]
msort xs = merge (msort $ fst halves) (msort $ snd halves)
    where halves = halve xs


-- Chapter 7: Higer-order functions

-- 7.9.1
filt p f xs = [f x | x <- xs, p x]
filt' p f   = map f . filter p 

-- 7.9.2
all' :: (a -> Bool) -> [a] -> Bool
all' f []     = True
all' f (x:xs) = f x && all' f xs

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = and . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f []     = False
any' f (x:xs) = f x || any' f xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' f = or . map f

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x       = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []     = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

-- 7.9.3
-- 7.9.4
-- 7.9.5









