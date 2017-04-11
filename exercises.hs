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
(||) :: Bool -> Bool -> Bool
False || False = False
False || True = True
True || False = True
True || True = True

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
and :: Bool -> Bool -> Bool
and x y = if x then (if y then True else False) else False

-- 4.8.6
and' :: Bool -> Bool -> Bool
and' x y = if x then y else False

-- 4.8.7











-- Chapter 5: List comprehensions
-- Chapter 6: Recursion
-- Chapter 7: Higer-order functions
