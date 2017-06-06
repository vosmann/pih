import Data.Char
import Data.List

-- Chapter 7: Higer-order functions

-- Binary string transmitter

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

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- Voting algorithms
-- First past the post

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Alternative vote

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Red", "Green"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (==[])

elim :: Eq a => a -> [[a]] -> [[a]]
elim b = map (filter (/=b)) 

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]    -> c
                (c:cs) -> winner' (elim c bs)

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
encode' :: String -> [Bit]
encode' = concat . map (parity . make8 . int2bin . ord)

parity :: [Bit] -> [Bit]
parity bits | (odd . length . filter (== 1)) bits = 1 : bits 
            | otherwise                           = 0 : bits

paritybit :: [Bit] -> Bit
paritybit bits = if (odd . length . filter (== 1)) bits then 1 else 0

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits : chop n (drop n bits)

decode' :: [Bit] -> String
decode' = map (chr . bin2int) . map check . chop 9

check :: [Bit] -> [Bit]
check bits = if paritybit databits == head bits then databits else error "parity error"
    where databits = tail bits

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
