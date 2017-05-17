module ChapterTen where

import Prelude hiding (getLine, putStr, putStrLn)
import System.IO hiding (getLine, putStr, putStrLn)

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


-- Life


