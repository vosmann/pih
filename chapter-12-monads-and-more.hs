import Control.Applicative hiding (Const, ZipList)

-- Chapter 12: Monads and more

-- 12.5.1
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    fmap g Leaf         = Leaf
    fmap g (Node l v r) = Node (fmap g l) (g v) (fmap g l)

-- 12.5.2
--instance Functor ((->) a) where
    -- fmap :: (* -> *) -> (a -> *) -> (a -> *)
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    -- fmap = (.)

-- 12.5.3
-- instance Applicative ((->) a) where
    -- pure :: b -> (a -> b)
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
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S(\s -> let (x, s') = app st s in app (f x) s')

instance Functor ST where
    -- fmap :: (a -> b) -> ST a          -> ST b
    -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
    fmap g st = do x <- st
                   return (g x)

instance Applicative ST where
    -- pure :: a -> ST a
    -- pure :: a -> (s -> (a, s))
    pure x = S (\s -> (x, s))

    -- (<*>) :: ST (a -> b)                 -> ST a          -> ST b
    -- (<*>) :: (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
    stf <*> stx = do f <- stf
                     x <- stx
                     return (f x)
