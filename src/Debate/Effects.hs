module Debate.Effects where

import Data.Char
import Control.Monad


mainAction = do
    -- putStrLn :: String -> IO ()
    discard <- putStrLn "Hello, what's your name?"
    -- discard :: ()
    -- getLine :: IO String
    name <- getLine
    let bigName = map toUpper name
    -- name :: String
    putStrLn $ "Hey " ++ (pureFunction bigName) ++ "Be careful!"


pureFunction :: String -> String
pureFunction x = x ++ ", you are inside an IO action. IO actions will only be performed when they are in main or when they are inside a bigger IO action that we composed with a do block. "

mainLoop :: IO ()
mainLoop = do
    putStrLn "Enter shit...Press RET to quit."
    line <- getLine
    -- null :: Foldable t => t a -> Bool
    if null line
    -- return () :: IO ()
    then do
        putStrLn "Enter more shit or press q to quit."
        prompt4Char
    else do
        putString $ reverseWords line
        putChar '\n'
        -- mainLoop :: IO ()
        mainLoop

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-- w/o composition: reverseWords st = unwords (map reverse (words st))

-- In IO do block, if..else must have the form:
-- if <cond> then <IO action> else <IO action>

-- return makes an IO action out of a pure value.
-- return () is a bogus IO action that doesn't do anything
-- but it is needed so both branches have the same type, IO ()


putString :: String -> IO ()
putString [] = return ()
putString (x:xs) = do
    putChar x
    putString xs


prompt4Char = do
    ch <- getChar
    if ch == 'q'
    then return ()
    else do
        putChar ch
        prompt4Char

{-
`when`, from Control.Monad, is a function that takes a boolean and an IO action. If the boolean is true, it returns that IO action, otherwise the `return ()` action.
-}
