module Types.Tuple where

data Tuple a b = Unit | Singleton a a | Pair a b deriving (Eq, Ord, Read, Show)
