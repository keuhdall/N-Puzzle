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

    --solve :: [Int] -> (Tree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> Tree) -> Tree