module Recreate.Option (
    Option(..)
) where

data Option a = None | Some a deriving (Eq, Ord, Read, Show)

isSome :: Option a -> Bool
isSome (Some a) =  True
isSome None     =  False

isNone :: Option a -> Bool
isNone = not . isSome

unwrap :: Option a -> String -> a
unwrap None s = error s
unwrap (Some x) _ = x

flattenOption :: Option (Option a) -> Option a
flattenOption None = None
flattenOption (Some (Some x)) = Some x

mapOrDefault :: b -> (a -> b) -> Option a -> b
mapOrDefault d _ None     = d
mapOrDefault _ f (Some x) = f x

chainOptions :: Option a -> Option b -> (a -> b -> b) -> Option b
chainOptions None     _        _ = None
chainOptions _        None     _ = None
chainOptions (Some x) (Some y) f = Some $ f x y


-- (<>) :: Semigroup a => a -> a -> a
-- (<>) :: Option a -> Option a -> Option a
-- instance Semigroup a => Semigroup (Option a) where
--     None     <> (Some y) = Some y
--     (Some x) <> None     = Some x
--     (Some x) <> (Some y) = Some y



-- fmap :: (a -> b) -> Option a -> Option b
-- fmap :: (a -> b) -> f a -> f b
instance Functor Option where
    fmap _  None    = None
    fmap f (Some x) = Some $ f x


-- (<*>):: Option (a -> b) -> Option a -> Option b
-- (<*>):: f (a -> b) -> f a -> f b
-- (*>) :: f a -> f b -> f b
-- (<*) :: f a -> f b -> f a
instance Applicative Option where
    pure  = Some
    Some f <*> sx = fmap f sx
    None   <*> _x = None


-- bind  :: Option a -> (a -> Option b) -> Option b
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>)  :: m a -> m b -> m b
instance Monad Option where
    return = Some
    Some x >>= f = f x
    _x     >>= _ = None




{-
    class Semigroup a where
        (<>) :: a -> a -> a

    class Semigroup a => Monoid a where
        mempty :: a
        mappend :: a -> a -> a
        mconcat :: [a] -> a
    MINIMAL: mempty

    instance Applicative Maybe where
        Just f  <*> m   = fmap f m
        Nothing <*> _m  = Nothing

        Just _m1 *> m2  = m2
        Nothing  *> _m2 = Nothing

        liftA2 f (Just x) (Just y) = Just (f x y)
        liftA2 _ _ _    = Nothing
    (<*>) | liftA2

    instance Monad Maybe where
        (Just x) >>= k = k x
        Nothing  >>= _ = Nothing
        (>>) = (*>)
-}
