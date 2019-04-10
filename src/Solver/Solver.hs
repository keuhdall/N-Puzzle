module Solver.Solver where
    import Data.Sort
    import Solver.BTree
    import Solver.Methods

    data SearchType = Astar | Uniform | Greedy deriving Eq

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    getSearchType :: String -> Maybe SearchType
    getSearchType s = case s of
        "astar"     -> Just $ Astar
        "uniform"   -> Just $ Uniform
        "greedy"    -> Just $ Greedy
        _           -> Nothing

    toString :: SearchType -> String
    toString st = case st of
        Astar   -> "astar"
        Uniform -> "uniform"
        Greedy  -> "greedy"

    getSolvedGrid :: [Int] -> [Int]
    getSolvedGrid xs = sort xs

    isSolved :: [Int] -> Bool
    isSolved xs = xs == getSolvedGrid xs

    getIndexes :: [Int] -> [(Int, Int)]
    getIndexes xs = let len = (length xs) - 1 in zip xs [0..len]

    getCoordinates :: [Int] -> Int -> (Int, Int)
    getCoordinates xs n = let size = floor $ sqrt $ fromIntegral $ length xs; x = n `mod` size; y = n `div` size in (x, y)

    --solve :: [Int] -> (Tree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> Tree) -> Tree