module DataStructs.Tree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Read, Show)

-- data PalmTree a = PalmLeaf a | PalmNode a (PalmTree a) (PalmTree a)

-- data PalmTree a = PalmLeaf a | PalmNode (PalmTree a) (PalmTree a)

-- data Tree a = Leaf a | Tree a :^: Tree a

-- Non-empty, possibly infinite, multi-way trees (aka rose tree)
data Rose a = Rosebud {
        rootLabel :: a,           -- label value
        subForest :: RoseBush a   -- zero or more child trees
    }

type RoseBush a = [Rose a]

mapRose :: (a -> b) -> Rose a -> Rose b
mapRose f (Rosebud x ts) = Rosebud (f x) (map (mapRose f) ts)


{-
  Data.Tree
   data Tree a = Node {rootLabel :: a, subForest :: Forest a}
    flatten        :: Tree a -> [a]
    levels         :: Tree a -> [[a]]
    foldTree       :: (a -> [b] -> b) -> Tree a -> b
    unfoldTree     :: (b -> (a, [b])) -> b -> Tree a
    unfoldTreeM    :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
    unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
    drawTree       :: Tree String -> String

   type Forest a = [Tree a]
    unfoldForest     :: (b -> (a, [b])) -> [b] -> Forest a
    unfoldForestM    :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
    unfoldForestM_BF :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
    drawForest       :: Forest String -> String

-}


insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node v west east)
    | x == v = Node v west east
    | x < v = Node v (insert x west) east
    | otherwise = Node v west (insert x east)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty tree = False

search :: (Ord a) => a -> Tree a -> Bool
search _ Empty = False
search x (Node y west east)
    | x == y = True
    | x < y  = search x west
    | otherwise = search x east

toString :: (Show a) => Tree a -> String
toString Empty = ""
toString (Node x west east)
    =  show x
    <> "("
    <> "\n"
    <> "\t"
    <> (toString west)
    <> ","
    <> (toString east)
    <> ")"


myShowTree :: (Show a) => Tree a -> String
myShowTree Empty
    = ""
    <> "\t"
myShowTree (Node root ls rs)
    = ""
    <> (show root)
    <> "\n"                 -- \n
    <> "\t"
    <> (aux ls)
    -- <> "\n"
    <> "\t"
    <> (aux rs)
    -- <> "\n"
    where
        aux Empty
            = ""
            <> "\t"
        aux (Node x west east)
            =  ""
            -- <> "\n"
            -- <> "\t"
            <> (show x)
            <> "\n"                 -- \n
            <> "\t"
            <> "\t"
            <> (myShowTree west)
            <> "\t"
            <> "\t"
            <> (myShowTree east)
            -- <> "\t"


showHelper :: Int -> String -> String
showHelper n x
    = ""
    <> (printTab n)
    <> show x

printTab :: Int -> String
printTab 0 = ""
printTab n = "\t" <> printTab (n - 1)




freeTree :: Tree String
freeTree =
    Node "P"
        (Node "L"
            (Node "LL"
                (Node "LLL" Empty Empty)
                (Node "LLR" Empty Empty)
            )
            (Node "LR"
                (Node "LRL" Empty Empty)
                (Node "LRR" Empty Empty)
            )
        )
        (Node "R"
            (Node "RL"
                (Node "RLL" Empty Empty)
                (Node "RLR" Empty Empty)
            )
            (Node "RR"
                (Node "RRL" Empty Empty)
                (Node "RRR" Empty Empty)
            )
        )

-- putStr $ toString freeTree
-- putStr $ myShowTree freeTree



{-
  For example, given the declarations:

    infixr 5 :^:
    data Tree a = Leaf a | Tree a :^: Tree a

  derived instance of Show is equivalent to:

    instance (Show a) => Show (Tree a) where
       showsPrec d (Leaf m) = showParen (d > app_prec) $
            showString "Leaf " . showsPrec (app_prec+1) m
         where app_prec = 10

       showsPrec d (u :^: v) = showParen (d > up_prec) $
            showsPrec (up_prec+1) u .
            showString " :^: "      .
            showsPrec (up_prec+1) v
         where up_prec = 5

  Note that right-associativity of :^: is ignored
    show (Leaf 1 :^: Leaf 2 :^: Leaf 3)
  produces the string
    "Leaf 1 :^: (Leaf 2 :^: Leaf 3)"
-}
