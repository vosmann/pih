

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

-- 15.9.6
