module Solver.Methods where
    import Solver.BTree
    import Solver.Distance

    data SearchType = Astar | Uniform | Greedy deriving Eq

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    instance Read SearchType where
        readsPrec _ "astar"     = [(Astar, "astar")]
        readsPrec _ "uniform"   = [(Uniform, "uniform")]
        readsPrec _ "greedy"    = [(Greedy, "greedy")]

    --astar :: Tree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> Tree