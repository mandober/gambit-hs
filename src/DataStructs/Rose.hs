module DataStructs.Rose where

import Data.Tree
-- data Tree a = Node { rootLabel :: a, subForest :: [Tree a] }

-- type Forest a = [Tree a]
-- data Tree a = Node { rootLabel :: a, subForest :: Forest a }

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node x xs) = f x : (concat $ map (preOrder f) xs)

-- preOrder show t2


t1 = Node 6 []
-- Node {rootLabel = 6, subForest = []}

t2 = Node 1 [
        Node 2 [
            Node 4 [],
            Node 5 []
            ],
        Node 3 [
            Node 6 []
            ]
    ]
-- [4,5,7,8,6,9]

t3 = Node 1 [ Node 2 [ Node 3 [], Node 4 [], Node 5 [] ], Node 6 [] ]
