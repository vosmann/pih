module ChapterTen where

import Prelude hiding (getLine, putStr, putStrLn)
import System.IO hiding (getLine, putStr, putStrLn)
import Data.Char

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

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board ]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
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

play' :: Board -> Int -> IO ()
play' board player = 
   do newline
      putBoard board
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


