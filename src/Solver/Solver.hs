module Solver.Solver where
    import Data.Sort
    import Solver.BTree
    import Solver.Methods

    data SearchType = Astor | Uniform | Greedy deriving Eq

    instance Show SearchType where
        show Astor      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    getSearchType :: String -> Maybe SearchType
    getSearchType = case s of
        "astor"     -> Just (Astor)
        "uniform"   -> Just (Uniform)
        "greedy"    -> Just (Greedy)
        _           -> Nothing

    toString :: SearchType -> String
    toString st = case st of
        Astor   -> "astor"
        Uniform -> "uniform"
        Greedy  -> "greedy"

    getSolvedGrid :: [Int] -> [Int]
    getSolvedGrid xs = sort xs

    solve :: [Int] -> (BTree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> BTree) -> BTree