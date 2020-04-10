module Recreate.List where

data List a = Empty | Cons a (List a) deriving (Eq, Ord, Read)
{-
  data [] a = [] | a : [a]          -- Defined in GHC.Types
    instance Eq   a => Eq   [a]     -- Defined in GHC.Classes
    instance Ord  a => Ord  [a]     -- Defined in GHC.Classes
    instance Show a => Show [a]     -- Defined in GHC.Show
    instance Read a => Read [a]     -- Defined in GHC.Read
    instance Semigroup [a]          -- Defined in GHC.Base
    instance Monoid [a]             -- Defined in GHC.Base
    instance Foldable []            -- Defined in Data.Foldable
    instance Functor []             -- Defined in GHC.Base
    instance Traversable []         -- Defined in Data.Traversable
    instance Applicative []         -- Defined in GHC.Base
    instance Monad []               -- Defined in GHC.Base
    instance MonadFail []           -- Defined in Control.Monad.Fail
-}

newList :: a -> List a
newList x = Cons x Empty

catLists :: List a -> List a -> List a
catLists Empty       ys = ys
catLists (Cons x xs) ys = Cons x (catLists xs ys)


lst0 = Empty
lst1 = Cons 1 (Cons 2 (Cons 3 (newList 4)))
lst2 = Cons 5 (Cons 6 (newList 7))


-- show :: Show a => List a -> String
instance Show a => Show (List a) where
    show xs = "List [" <> aux xs <> "]" where
        aux Empty = ""
        aux (Cons x Empty) = show x
        aux (Cons x xs)    = show x <> ", " <> aux xs


-- fmap :: (a -> b) -> [a] -> [b]
-- fmap :: (a -> b) -> List a -> List b
instance Functor List where
    fmap _ Empty = Empty
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)



_ = fmap (+5) lst1  -- [9, 8, 7, 6]
_ = fmap (*3) lst0  -- []


-- pure :: a -> f a
-- (<*>):: f (a -> b) -> f a -> f b
-- (<*>):: List (a -> b) -> List a -> List b
instance Applicative List where
    pure = newList
    Empty          <*> _    = Empty
    (Cons f Empty) <*> list = fmap f list

-- instance Applicative [] where
    -- fs <*> xs = [f x | f <- fs, x <- xs]
    -- xs *> ys  = [y | _ <- xs, y <- ys]
    -- liftA2 f xs ys = [f x y | x <- xs, y <- ys]

_ = pure (+5)    <*> lst0    -- []
_ = pure (*10)   <*> lst1    -- [40, 30, 20, 10]
_ = newList (^2) <*> lst2    -- [9, 4, 1]
_ = Empty        <*> lst2    -- []



-- bind  :: List a -> (a -> List b) -> List b
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>)  :: m a -> m b -> m b
instance Monad List where
    return = newList
    Empty       >>= _ = Empty
    (Cons x xs) >>= f = f x `catLists` (xs >>= f)


mr0 = lst0 >>= (\x -> newList (x + 5))  -- List []
mr1 = lst2 >>= (\x -> newList (x * 3))  -- List [15, 18, 21]


_ = [1,2,3] >>= (\x -> [x+3])     -- [4,5,6]
_ = "abcde" >>= (\c -> [c,c])     -- "aabbccddee"
_ = (\c -> [c,c]) `fmap` "abcde"    -- ["aa","bb","cc","dd","ee"]



{-

intersperse :: a -> [a] -> [a]
intersperse '.' "MONKEY"                    -- "M.O.N.K.E.Y"
intersperse 0 [1,2,3,4,5,6]                 -- [1,0,2,0,3,0,4,0,5,0,6]
pure (intersperse '~') <*> ["abcde"]        -- ["a~b~c~d~e"]

intercalate :: [a] -> [[a]] -> [a]
intercalate " " ["hey","there","guys"]      -- "hey there guys"
intercalate [0] [[1,2,3],[4,5,6],[7,8,9]]   -- [1,2,3,0,4,5,6,0,7,8,9]

transpose :: [[a]] -> [[a]]
transpose [[1,2,3],[4,5,6],[7,8,9]]         -- [[1,4,7],[2,5,8],[3,6,9]]
transpose ["hey","there","guys"]            -- ["htg","ehu","yey","rs","e"]




https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.13.0.0/src/GHC-Base.html#map

class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) ::       b  -> f a -> f b
    (<$) = fmap . const
                  const :: a -> b -> a


class Functor f => Applicative f where

MINIMAL: pure, <*>|liftA2
    pure :: a -> f a

    -- Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id

    -- Lift a binary function to actions
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x = (<*>) (fmap f x)

    -- Sequence actions, discarding the value of the first argument.
    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2
    -- This is essentially the same as liftA2 (flip const), but if the
    -- Functor instance has an optimized (<$), it may be better to use
    -- that instead. Before liftA2 became a method, this definition
    -- was strictly better, but now it depends on the functor. For a
    -- functor supporting a sharing-enhancing (<$), this definition
    -- may reduce allocation by preventing a1 from ever being fully
    -- realized. In an implementation with a boring (<$) but an optimizing
    -- liftA2, it would likely be better to define (*>) using liftA2.

    -- Sequence actions, discarding the value of the second argument.
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const


The 'join' function is the conventional monad join operator. It is used to remove one level of monadic structure, projecting its bound argument into the outer level.

==== Examples

A common use of 'join' is to run an 'IO' computation returned from an 'GHC.Conc.STM' transaction, since 'GHC.Conc.STM' transactions can't perform 'IO' directly. Recall that:

GHC.Conc.atomically :: STM a -> IO a

is used to run 'GHC.Conc.STM' transactions atomically. So, by specializing the types of 'GHC.Conc.atomically' and 'join' to

GHC.Conc.atomically :: STM (IO b) -> IO (IO b)
join :: IO (IO b)  -> IO b

we can compose them as
join . GHC.Conc.atomically :: STM (IO b) -> IO b

to run an 'GHC.Conc.STM' transaction and the 'IO' action it returns.

join   :: (Monad m) => m (m a) -> m a
join x =  x >>= id


The 'Monad' class defines the basic operations over a monad, a concept from a branch of mathematics known as category theory. From the perspective of a Haskell programmer, however, it is best to think of a monad as an abstract datatype of actions. Haskell's do-expressions provide a convenient syntax for writing monadic expressions.

Instances of 'Monad' should satisfy the following:
* Left identity : return a >>= k = k a
* Right identity: m >>= return = m
* Associativity : m >>= (\x -> k x >>= h) = (m >>= k) >>= h

Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:
* pure = return
* (<*>) = ap

The above laws imply:
* fmap f xs = xs >>= return . f
* (>>) = (*>)

and that 'pure' and (<*>) satisfy the applicative functor laws.


The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the "Prelude" satisfy these laws.



class Applicative m => Monad m where

    -- Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    -- Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]

    -- Inject a value into the monadic type.
    return      :: a -> m a
    return      = pure



Note Recursive bindings for Applicative/Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The original Applicative/Monad proposal stated that after implementation, the designated implementation of (>>) would become

  (>>) :: forall a b. m a -> m b -> m b
  (>>) = (*>)

by default. You might be inclined to change this to reflect the stated proposal, but you really shouldn't! Why? Because people tend to define such instances the other way around: in particular, it is perfectly legitimate to define an instance of Applicative (*>) in terms of (>>), which would lead to an infinite loop for the default implementation of Monad! And people do this in the wild.

This turned into a nasty bug that was tricky to track down, and rather than eliminate it everywhere upstream, it's easier to just retain the original default.

Same as >>= but with the arguments interchanged:
-- SPECIALISE (=<<) :: (a -> [b]) -> [a] -> [b]
(=<<)           :: Monad m => (a -> m b) -> m a -> m b
f =<< x         = x >>= f



The MonadPlus class definition
==============================

Monads that also support choice and failure.

class (Alternative m, Monad m) => MonadPlus m where
   -- The identity of mplus
   -- It should also satisfy the equations:
   --   mzero >>= f  =  mzero
   --   v >> mzero   =  mzero

   -- default definition
   mzero :: m a
   mzero = empty

   -- associative operation
   mplus :: m a -> m a -> m a
   mplus = (<|>)




The list type
=============

instance Monad [] where
    xs >>= f = [y | x <- xs, y <- f x]
    (>>) = (*>)

instance Alternative [] where
    empty = []
    (<|>) = (++)

instance MonadPlus []

A few list functions that appear here because they are used here.
The rest of the prelude list functions are in GHC.List.


-}
