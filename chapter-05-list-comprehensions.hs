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
find' :: Eq a => a -> [(a, b)] -> [b]
find' k t = [v' | (k', v') <- t, k == k' ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find' x $ zip xs [0..]

-- 5.7.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
