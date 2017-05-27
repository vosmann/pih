import Control.Applicative
import Data.Char
import System.IO

-- Chapter 13: Monadic parsing

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of 
                      []     -> []
                      (x:xs) -> [(x,xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g (P p) = P (\inp -> case p inp of
                                  []         -> []
                                  [(v, out)] -> [(g v, out)])
    -- Or using function "parse":
    -- fmap g p = P (\inp -> case parse p inp of
    --                               []         -> []
    --                               [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                               [] -> []
                               [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
    return = pure
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= f = P (\inp -> case parse px inp of
                              [] -> []
                              [(x, out)] -> parse (f x) out)

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x, z)

three' :: Parser (Char, Char)
three' = do x <- item
            item
            z <- item
            return (x, z)
        where g x y z = (x, z)

instance Alternative Parser where
    -- empty :: a -> Parser a
    empty = P (\_ -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                             []         -> parse q inp       
                             [(v, out)] -> [(v, out)])
 
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char 
lower = sat isLower

upper :: Parser Char 
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char 
char c = sat (== c)

string :: String -> Parser String
string []     = []
string (x:xs) = 

-- Arithmetic expressions


-- Calculator

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x



-- Exercises

-- 13.11.1
-- 13.11.2
-- 13.11.3
-- 13.11.4
