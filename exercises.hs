--import Prelude hiding (getLine, putStr, putStrLn)
--import System.IO hiding (getLine, putStr, putStrLn)
import Prelude hiding (putStr)
import System.IO hiding (putStr)
import Data.Char
import Data.Ord
import Control.Applicative hiding (Const, ZipList)

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
find' :: Eq a => a -> [(a, b)] -> [b]
find' k t = [v' | (k', v') <- t, k == k' ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find' x $ zip xs [0..]

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
-- a
all' :: (a -> Bool) -> [a] -> Bool
all' f []     = True
all' f (x:xs) = f x && all' f xs

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = and . map f

-- b
any' :: (a -> Bool) -> [a] -> Bool
any' f []     = False
any' f (x:xs) = f x || any' f xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' f = or . map f

-- c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x       = x : takeWhile' f xs
                    | otherwise = []

-- d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []     = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

-- 7.9.3
mapf f = foldr (\x xs -> f x : xs) []

filterp p = foldr (\x xs -> if p x then x : xs else xs) []

-- 7.9.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- 7.9.5
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

-- 7.9.6
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin' = unfold (== 0) (`mod` 2) (`div` 2)
chop8' = unfold null (take 8) (drop 8)
mapf' f = unfold null (f . head) tail
iterate' f = unfold (\_ -> False) id f

-- 7.9.7
type Bit = Int
weights = map (2^) [0,1..]

bin2int' :: [Bit] -> Int
bin2int' bits = sum [ w*b | (w, b) <- zip weights bits]

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit] 
int2bin 0 = []
int2bin x = (x `mod` 2) : int2bin (x `div` 2)

make8 :: [Bit] -> [Bit] 
make8 bits = take 8 $ bits ++ repeat 0

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

encode' :: String -> [Bit]
encode' = concat . map (parity . make8 . int2bin . ord)

parity :: [Bit] -> [Bit]
parity bits | (odd . length . filter (== 1)) bits = 1 : bits 
            | otherwise                           = 0 : bits

paritybit :: [Bit] -> Bit
paritybit bits = if (odd . length . filter (== 1)) bits then 1 else 0

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits : chop n (drop n bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

decode' :: [Bit] -> String
decode' = map (chr . bin2int) . map check . chop 9

check :: [Bit] -> [Bit]
check bits = if paritybit databits == head bits then databits else error "parity error"
    where databits = tail bits

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- 7.9.8
faultychannel :: [Bit] -> [Bit]
faultychannel = tail

transmit' :: String -> String
transmit' = decode' . faultychannel . encode'

-- 7.9.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []     = []
altMap f g (x:xs) = f x : altMap g f xs
-- altMap (+10) (+100) [0,1,2,3,4] = [10,101,12,103,14]

-- 7.9.10
luhn' :: [Int] -> Bool
luhn' = (==0) . (`mod` 10) . sum . altMap id (subtract . (*2)) . reverse
    where subtract n | n > 9     = n - 9
                     | otherwise = n
--luhn' [1,5,6,7,6,6,6,4,4] == True
--luhn' [4,9,9,2,7,3,9,8,7,1,6] == True


-- Chapter 8: Declaring types and classes

-- 8.9.1
data Nat = Zero | Succ Nat
    deriving (Eq, Ord, Show)

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult'' :: Nat -> Nat -> Nat
mult'' Zero     n = Zero
mult'' (Succ m) n = add n (mult'' m n)

-- 8.9.2
data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node left y right) | x == y    = True
                             | x < y     = occurs x left
                             | otherwise = occurs x right

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node left y right) = case compare x y of
                                  LT -> occurs' x left
                                  EQ -> True
                                  GT -> occurs' x right
-- occurs' 2 (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)))
-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering

-- 8.9.3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
    deriving (Eq, Ord, Show)

nrleaves :: Tree' a -> Int
nrleaves (Leaf' n)   = 1
nrleaves (Node' l r) = nrleaves l + nrleaves r

balanced :: Tree' a -> Bool
balanced (Leaf' n)   = True
balanced (Node' l r) = abs (nrleaves l - nrleaves r) <= 1 && balanced l && balanced r

-- 8.9.4
balance :: [Int] -> Tree' Int
balance []    = error "Cannot transform empty list to tree."
balance [x]   = Leaf' x
balance [x,y] = Node' (Leaf' x) (Leaf' y)
balance xs    = Node' (balance (fst halves)) (balance (snd halves))
    where halves = halve xs

-- 8.9.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n)   = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 8.9.6
eval' :: Expr -> Int
eval' = folde id (\x y -> x+y)

size :: Expr -> Int
size = folde (\_ -> 1) (\x y -> x+y) 

-- 8.9.7
data Maybe a = Nothing | Just a
instance Eq a => Eq (Main.Maybe a) where
    Main.Nothing == Main.Nothing = True
    Main.Nothing == Main.Just x = False
    Main.Just x == Main.Nothing = False
    Main.Just x == Main.Just y = x == y

--instance Eq a => Eq ([a]) where
--    [] == []     = True
--    (x:xs) == [] = False
--    [] == (y:ys) = False
--    (x:xs) == (y:ys) = (x == y) && xs == ys

-- 8.9.8
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B')) 

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k,v)]
find :: Eq a => a -> Assoc a b -> b
find k t = head [v | (k',v) <- t, k'==k]

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval s (Const c) = c
eval s (Var c) = find c s
eval s (Not p) = not (eval s p) 
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var c)     = [c]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) onelessbools ++ map (True:) onelessbools
    where onelessbools = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

p5 = Or (Var 'A') (Not (Var 'A')) 

-- 8.9.9


-- Chapter 10: Interactive programming

-- 10.10.1
putStr :: String -> IO ()
putStr xs = sequence_ [putChar x | x <- xs]

-- 10.10.2
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

type Board' = [Int]

initial :: Board'
initial = [7,6,5,4,3,2]

putBoard :: Board' -> IO ()
putBoard ns = putBoardHelp ns 1

putBoardHelp :: Board' -> Int -> IO ()
putBoardHelp ns row | row <= length ns =  do putRow row (ns !! (row-1))
                                             putBoardHelp ns (row+1)
                    | otherwise        = return ()

-- 10.10.3
putBoard' :: Board' -> IO ()
putBoard' ns = sequence_ [putRow r n | (r,n) <- zip [1..] ns]

-- 10.10.4
adder :: IO ()
adder = do putStr "Input summand count: "
           r <- getNumber
           adder' 0 r

getNumber :: IO Int
getNumber =  do s <- getLine
                if all isDigit s then
                    return (read s)
                else
                    error "Numbers should consist only of digits."

adder' :: Int -> Int -> IO ()
adder' s r = if r > 0 then
                do num <- getNumber
                   adder' (s+num) (r-1)
             else 
                do putStr "Sum: "
                   putStrLn (show s)


-- 10.10.5
adderSeq :: IO ()
adderSeq = do putStr "Input summand count: "
              r    <- getNumber
              nums <- sequence $ take r $repeat getNumber
              putStr "Sum: "
              putStrLn (show (sum nums))

-- 10.10.6
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = readLine' ""

readLine' :: String -> IO String
readLine' cs = do c <- getCh
                  case c of
                      '\DEL' -> do putChar '\b'
                                   readLine' (dropLast cs)
                      '\n'   -> do putStr "Read line: "
                                   return cs
                      _      -> do putChar c 
                                   readLine' (cs++[c])

dropLast xs = if null xs then [] else init xs


-- Chapter 12: Monads and more

-- 12.5.1
data Tree'' a = Leaf'' | Node'' (Tree'' a) a (Tree'' a)
    deriving Show

instance Functor Tree'' where
    -- fmap :: (a -> b) -> f a -> f b
    fmap g Leaf''         = Leaf''
    fmap g (Node'' l v r) = Node'' (fmap g l) (g v) (fmap g l)

-- 12.5.2
-- instance Functor ((->) a) where
       -- fmap :: (* -> *) -> (a -> *) -> (a -> *)
       -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--     fmap = (.)

-- 12.5.3
-- instance Applicative ((->) a) where
    -- pure :: b -> a -> b
    -- pure    = const
    -- (<*>) :: (a -> (* -> *)) -> (a -> *) -> (a -> *)
    -- (<*>) :: (a ->  b -> c)  -> (a -> b) -> (a -> c)
    -- g <*> h = \x -> g x (h x)


-- 12.5.4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)
    -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

-- 12.5.5
-- Applicative laws and types of their variables

-- 1.
-- pure id <*> x   = x
--
-- :t x
-- f x

-- 2.
-- pure (g x)      = pure g <*> pure x
--
-- :t g
-- (a -> b)
-- :t x
-- a

-- 3.
-- x <*> pure y    = pure (\g -> g y) <*> x
--
-- :t x
-- f (a -> b)
-- :t y
-- a
-- :t g
-- a -> b

-- 4.
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
--
-- :t x
-- TODO
-- :t y
-- TODO
-- :t z
-- TODO

-- 12.5.6
-- instance Monad ((->) a) where
    -- pure :: b -> a -> b
    -- pure    = const
    -- return  = pure
    -- (>>=) :: (a -> *) -> (* -> (a -> **)) -> (a -> **)
    -- (>>=) :: (a -> b) -> (b -> (a -> c )) -> (a -> c )
    -- (>>=) :: (a -> b) -> (b ->  a -> c  ) -> (a -> c )
    -- (>>=) :: m b      -> (b ->  m c     ) -> m c       -- m x is a function
    -- (>>=) :: f        -> (y ->  g       ) -> g         -- m b = f, m c = g, b -> a -> c = h
    -- \x -> y >>= \y -> (\x -> z) = \x -> z
    -- f       >>= h               = \u -> h (f u) u

-- 12.5.7
data Expression a = Variable a | Value Int | Addition (Expression a) (Expression a)
                    deriving Show

instance Functor Expression where
    -- fmap :: (a -> b) -> Expression a -> Expression b
    fmap g (Variable x)   = Variable (g x)
    fmap g (Value i)      = Value i
    fmap g (Addition x y) = Addition (fmap g x) (fmap g y)

instance Applicative Expression where
    -- pure  :: a -> Expression a
    pure x  = Variable x
    -- (<*>) :: Expression (a -> b) -> Expression a -> Expression b
    Variable f <*> x       = fmap f x
    Value x    <*> Value y = Addition (Value x) (Value y)

instance Monad Expression where
    -- (>>=) :: Expression a -> (a -> Expression b) -> Expression b
    Value x      >>= f = Value x
    Variable x   >>= f = f x
    Addition x y >>= f = Addition (x >>= f) (y >>= f)

-- 12.5.8



