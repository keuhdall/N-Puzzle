module Methods where
    import Solver.BTree
    import Solver.Distance

    astar :: BTree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> BTree