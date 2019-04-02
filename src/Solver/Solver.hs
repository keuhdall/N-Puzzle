module Solver.Solver where
    import Data.Sort
    import Solver.BTree
    import Solver.Methods

    getSolvedGrid :: [Int] -> [Int]
    getSolvedGrid xs = sort xs

    solve :: [Int] -> (BTree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> BTree) -> BTree