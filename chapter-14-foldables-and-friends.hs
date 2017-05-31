import Prelude hiding (foldl)
import Data.Maybe


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


-- 14.5.4
-- 14.5.5
