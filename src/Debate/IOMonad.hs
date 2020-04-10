{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Debate.IOMonad where
{-
What is IO monad, 2018
======================
https://www.youtube.com/watch?v=fCoQb-zqYDI
https://github.com/rexim/io

-}
import Control.Monad
-- import Data.Array.IO
import Data.Foldable
import System.IO.Unsafe

-- infixl 1  >>>=


-- represents the state of the world
data World = World deriving Show

printStr :: String -> World -> World
printStr s !w = unsafePerformIO (putStrLn s >> return w)

readStr :: World -> (String, World)
readStr !w = unsafePerformIO (getLine >>= \s -> return (s, w))

whatIsYourPureName :: World -> World
whatIsYourPureName w1 = w4
    where
      w2         = printStr "What is your name?" w1
      (name, w3) = readStr w2
      w4         = printStr ("Hello " ++ name) w3

ww1 = whatIsYourPureName World

branch :: World -> (World, World)
branch w = (printStr "True world" w, printStr "False world" w)

ww2 = branch World


{-
uniqueness type
===============

-- Just a dummy marker to mark World as Unique
data Unique a = Unique a

-- printStr :: String -> World -> World
printStrU :: String -> Unique World -> Unique World
printStrU text (Unique w) = Unique (printStr text w)

-- If we had unique types this would not compile
branchUnique :: Unique World -> (Unique World, Unique World)
branchUnique w = (printStrU "True world" w,
                  printStrU "False world" w)

noBranching :: Unique World -> Unique World
noBranching w1 = w3
    where w2 = printStrU "I love you" w1
          w3 = printStrU "False world" w2
-}

-- Type synonym to hide the World
type WorldT a = World -> (a, World)

--          World -> (String, World)
readStrT :: WorldT String
readStrT = readStr

--           String -> World -> ((), World)
printStrT :: String -> WorldT ()
printStrT s w = ((), printStr s w)

-- We can now call readStrT by supplying it a World:
--readStrT World
-- > 344            -- this is out keyboard input
-- ("344",World)    -- the return value

-- We can now also use printStrT by supplying it a string and a World:
--printStrT "Hi" World
-- ((),Hi  -- "Hi" is the printed string that got mixed
-- World)  -- with the return value of `((), World)`
