module Types.IO where

{-
https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.13.0.0/src/GHC-Base.html#map

----------------------------------------------
-- Functor/Applicative/Monad instances for IO
----------------------------------------------


-- | @since 2.01
instance  Functor IO where
   fmap f x = x >>= (pure . f)

-- | @since 2.01
instance Applicative IO where
    pure  = returnIO
    (*>)  = thenIO
    (<*>) = ap
    liftA2 = liftM2

-- | @since 2.01
instance  Monad IO  where
    (>>)      = (*>)
    (>>=)     = bindIO

-- | @since 4.9.0.0
instance Alternative IO where
    empty = failIO "mzero"
    (<|>) = mplusIO

-- | @since 4.9.0.0
instance MonadPlus IO

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO (\ s -> case m s of (# new_s, _ #) -> unIO k new_s)

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a




-}
