module Lambda.Calculus where

idiot :: a -> a
idiot = \x -> x

kestrel :: a -> b -> a
kestrel = \x -> \y -> x

bluebird :: (b -> c) -> (a -> b) -> (a -> c)
bluebird = \g -> \f -> \x -> g (f x)

-- starling :: (a -> a) -> (a -> a) -> (a -> a) -> (a -> a)
-- starling = \g -> \f -> \x -> (g x) . (f x)

-- star :: (a -> a) -> (a -> a) -> a -> a
-- star = \g -> \f -> \x -> (g x) . (f x)
