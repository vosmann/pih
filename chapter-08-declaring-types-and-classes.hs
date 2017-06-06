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

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
           where half = length xs `div` 2

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
