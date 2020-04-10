module Types.Functions where

fx :: (a -> b) -> a -> b
f `fx` x = f x

xf :: a -> (a -> b) -> b
x `xf` f = f x

identity :: a -> a
identity x = x

constant :: a -> b -> a
constant x _ = x

{-
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

applyN :: Int -> (a -> a) -> a -> a
applyN n f = X.foldr (.) identity (X.replicate n f)

print :: (MonadIO m, Show a) => a -> m ()
print = io . putStrLn . tshow

io :: MonadIO m => IO a -> m a
io = IO.liftIO


Functor
=======

-- Functions are Functors too.
-- fmap on a function is function composition

https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.13.0.0/src/GHC-Base.html#map

instance Functor ((->) r) where
    fmap f g = f . g

instance Functor ((->) r) where
    fmap = (.)


instance Applicative ((->) a) where
    pure = const
    (<*>) f g x = f x (g x)
    liftA2 q f g x = q (f x) (g x)

instance Monad ((->) r) where
    f >>= k = \ r -> k (f r) r

instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

-}
