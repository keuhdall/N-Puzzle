module PQueue.PQueue () where
    data Item a = Item {
        content :: a,
        priority :: Int
    } deriving (Show, Eq, Ord)

    type PQueue a = [Item a]

    sortItems :: PQueue a -> PQueue a
    sortItems []     = []
    sortItems (p:xs) = (sortItems lesser) ++ [p] ++ (sortItems greater) where
        prio = (priority p)
        lesser  = filter (< prio) xs
        greater = filter (>= prio) xs

    insert :: a -> PQueue a -> PQueue a
    insert x xs = sortItems (x:xs)

    getFirst :: PQueue a -> (PQueue a, Maybe a)
    getFirst [] = ([], Nothing)
    getFirst xs = let xs' = tail xs; a = Just (head xs) in (xs' a)

    getLast :: PQueue a -> (PQueue a, Maybe a)
    getLast [] = ([], Nothing)
    getLast xs = let xs' = init xs; a = Just (last xs) in (xs' a)