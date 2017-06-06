import Prelude hiding (getLine, putStr, putStrLn)
import System.IO hiding (getLine, putStr, putStrLn)
import Data.Char

-- Chapter 10: Interactive programming

getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                 return []
             else
                 do xs <- getLine
                    return (x:xs)

putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do putChar x
                   putStr xs

putStrLn :: String -> IO ()
putStrLn s = do putStr s
                putChar '\n'
             

strlen :: IO()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters."

-- Hangman
hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess the word."
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                  do putChar x
                     return []
              else
                  do putChar '-'
                     xs <- sgetLine 
                     return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word = do guess <- getLine
               if guess == word then
                   putStrLn "You guessed right."
               else
                   do putStrLn (match word guess)
                      play word

match :: String -> String -> String
match [] guess = []
match (w:ws) guess = (if w `elem` guess then w else '-') : (match ws guess)
--match word guess = [if clear `elem` guess then clear else '-' | clear <- word]

-- Nim
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board' = [Int]

initial :: Board'
initial = [5,4,3,2,1]

finished :: Board' -> Bool
finished = all (== 0)

valid :: Board' -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board' -> Int -> Int -> Board'
move board row num = [update r n | (r, n) <- zip [1..] board ]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putFiveRowBoard :: Board' -> IO ()
putFiveRowBoard [a,b,c,d,e] = do putRow 1 a
                                 putRow 2 b
                                 putRow 3 c
                                 putRow 4 d
                                 putRow 5 e

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "Error: Invalid digit."
                           getDigit prompt

play' :: Board' -> Int -> IO ()
play' board player = 
   do newline
      putFiveRowBoard board
      if finished board then
         do newline
            putStr "Player "
            putStr (show (next player))
            putStrLn " wins."
      else
         do newline
            putStr "Player "
            putStr (show player)
            row <- getDigit "Enter row number: "
            num <- getDigit "Enter number of stars to remove: "
            if valid board row num then
               play' (move board row num) (next player)
            else
               do newline
                  putStrLn "Error: Invalid move."
                  play' board player

nim :: IO ()
nim = play' initial 1


-- Life
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1),
                          (x,y+1), (x+1,y+1)] 

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)), 
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]


-- 10.10.1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- 10.10.2
initial' :: Board'
initial' = [7,6,5,4,3,2]

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



