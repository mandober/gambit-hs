module Recreate.Result where

data Result a b = Err a | Ok b deriving (Eq, Ord, Read, Show)

isErr :: (Result a b) -> Bool
isErr (Err _) = True
isErr (Ok _)  = False

rer = Err "An error occured" :: Result String Int
rok = Ok 333 :: Result String Int


-- fmap will ignore Err but map a fn over Ok
instance Functor (Result a) where
    fmap _ (Err e) = Err e
    fmap f (Ok x)  = Ok $ f x


res1 = fmap (*2) rer -- Left "foo"
res2 = fmap (*2) rok -- Right 6



-- Monad instance allows chaining of fallible actions;
-- they should fail overall if (and asap) any individual step fails.
-- First we write a fn that can either parse an Int from a Char or fail
