module Debate.Monads2 where
{-
Haskell Programming From First Principles
18.4 Examples of Monad use


-}
twiceWhenEven xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else [x*x]

_ = twiceWhenEven [1,2,3,4,5]  -- [1,4,4,9,16,16,25]


twe lst = [ [x*x] | x <- lst, x > 2 ]
