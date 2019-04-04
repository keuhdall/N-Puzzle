module BTree where
    data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

    -- Utility function that create a singleton tree
    singleton :: a -> Tree a
    singleton x = Node x EmptyTree EmptyTree

    -- Function that insert an element within the given btree
    insert :: (Ord a) => a -> Tree a -> Tree a
    insert x EmptyTree = singleton x
    insert x (Node a l r)
        | x == a    = Node x l r
        | x < a     = Node a (insert x l) r
        | x > a     = Node a l (insert x r)