{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parsers.Parsec where

-- Monadic Parser Combinators
-- http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

import Data.Char

-- cannot use type synonim as class instance:
-- type Parser a = String -> [(a,String)]
-- better to use fully defined type:
data Parser a = Pars (String -> [(a, String)])


result :: a -> Parser a
result v = Pars (\inp -> [(v,inp)])

-- char :: Parser Char
-- char [] = Pars (\inp -> [])
-- char (x:xs) = Pars (\inp -> [(x,xs)])
