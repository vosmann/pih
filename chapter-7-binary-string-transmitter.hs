import Data.Char
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
