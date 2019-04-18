module PQueue.PQueue (Item, insert, getFirst, getLast) where
    data Item a = Item {
        content :: a,
        priority :: Int
    } deriving (Show, Eq, Ord)

    type PQueue a = [Item a]

    sortItems :: PQueue a -> PQueue a
    sortItems []     = []
    sortItems (p:xs) = (sortItems lesser) ++ [p] ++ (sortItems greater) where
        prio = (priority p)
        lesser  = filter (\x -> (priority x) < prio)  xs
        greater = filter (\x -> (priority x) >= prio) xs

    insert :: Item a -> PQueue a -> PQueue a
    insert x xs = sortItems (x:xs)

    getFirst :: PQueue a -> (PQueue a, Maybe (Item a))
    getFirst [] = ([], Nothing)
    getFirst xs = let xs' = tail xs; x = Just (head xs) in (xs', x)

    getLast :: PQueue a -> (PQueue a, Maybe (Item a))
    getLast [] = ([], Nothing)
    getLast xs = let xs' = init xs; x = Just (last xs) in (xs', x)