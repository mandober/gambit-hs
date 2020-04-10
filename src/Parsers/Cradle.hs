module Parsers.Cradle where

import Data.Char

{-
program  := lexeme | whitespace
expr     := term { binOp term }


Given two recognizers p and q, we can define two major parser combos:

* ⊕ ALTERNATIVE PARSER COMBINATOR
  applies both to the same input and sums up the results returned by both, which is eventually returned as the final result:
  p ⊕ q (j) = p(j) ∪ q(j)

* ⊛ SEQUENCER OF RECOGNIZERS
  applies p to the input, if successful then q is applied to every element of the result set returned by p. ⊛ ultimately returns the union of these applications of q:
  (p ⊛ q) (j) = U { q(k):k ∈ p(j) }

-}

expected :: String -> String
expected t = error $ t ++ " expected"

actual :: Char -> String -> String
actual char msg = "Got char " ++ (show char) ++ " that is " ++ msg

eatDigit :: Char -> String
eatDigit char
    | isDigit char = actual char "digit"
    | otherwise = expected "digit"

term :: Char -> [Char]
term x
  | isDigit x = [x]
  | otherwise = expected "digit"

binOp :: Char -> [Char]
binOp '+' = "+"
binOp '-' = "-"
binOp _ = expected "binOp"

expr :: [Char] -> [Char]
expr [] = ""
expr (x1:[]) = term x1
expr (x1:x2:xs) = (term x1) ++ (binOp x2) ++ (expr xs)


{-
expr := term { binOp term }

A recursive BNF grammer definition leads to a recursive data type definition:
the definiton says that we can have a single number (term), or we can add or subtract (as binOp) any two (sub)terms.

-}

data Expr =
    Num Int
    | Add Expr Expr
    | Sub Expr Expr
    deriving (Read, Show)
