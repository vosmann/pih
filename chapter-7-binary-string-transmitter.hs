import Data.Char

type Bit = Int

weights = map (2^) [0,1..]

bin2int' :: [Bit] -> Int
bin2int' bits = sum [ w*b | (w, b) <- zip weights bits]
--   [a,b,c,d]
-- = 1*a + 2*b + 4*c + 8*d 
-- = a + 2*(b + 2*c + 4*d) 
-- = a + 2*(b + 2*(c + 2*d) 
-- = a + 2*(b + 2*(c + 2*(d + 2*0)) 

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
