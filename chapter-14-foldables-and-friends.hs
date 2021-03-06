import Prelude hiding (foldl)
import Data.Maybe
import Control.Applicative


-- Chapter 14: Foldables and friends


class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

class Foldable t where
    fold    :: Monoid a => t a -> a
    foldMap :: Monoid b => (a -> b) -> t a -> b
    -- Commented out foldr to not clash with the definition imported and used in mconcat.
    -- foldr   :: (a -> b -> b) -> b -> t a -> b
    foldl   :: (a -> b -> a) -> a -> t b -> a

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- 14.5.1
instance (Monoid a, Monoid b) => Monoid (a,b) where
    -- mempty :: (a,b)
    mempty = (mempty, mempty)
    -- mappend :: (a,b) -> (a,b) -> (a,b)
    mappend (x1,y1) (x2,y2) = (mappend x1 x2, mappend y1 y2)

-- 14.5.2
instance Monoid b => Monoid (a -> b) where
    -- mempty :: a -> b
    mempty = \_ -> mempty
    -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
    mappend = \f g -> (\x -> (f x `mappend` g x))

-- 14.5.3
instance Foldable Maybe where
    -- fold    :: Monoid a => Maybe a -> a
    fold Nothing  = mempty
    fold (Just x) = x

    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap _ Nothing  = mempty
    foldMap f (Just x) = f x

    -- foldl   :: (a -> b -> a) -> a -> Maybe b -> a
    foldl _ v Nothing  = v
    foldl f v (Just x) = f v x

instance Traversable Maybe where
    -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse g Nothing  = pure Nothing
    traverse g (Just x) = pure Just <*> (g x)

-- 14.5.4
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    fmap g Leaf         = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g l)

instance Foldable Tree where
    -- fold    :: Monoid a => Tree a -> a
    fold Leaf         = mempty
    fold (Node l v r) = (fold l) `mappend` v `mappend` (fold r)

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap _ Leaf         = mempty
    foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

    -- foldl   :: (a -> b -> a) -> a -> Tree b -> a
    foldl _ v Leaf         = v
    foldl f v (Node l x r) = foldl f (foldl f (f v x) l) r

instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g Leaf         = pure Leaf
    traverse g (Node l x r) = pure Node <*> (traverse g l) <*> (g x) <*> (traverse g r)
    -- traverse (Just . (*1000)) (Node (Node (Leaf) 2 (Leaf)) 1 (Node (Leaf) 3 (Leaf)))

-- 14.5.5
instance Monoid [a] where
    mempty  = []
    mappend = (++)

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p foldable = filter p (foldMap (\x -> [x]) foldable)
