module Solver.Methods where
    import Data.PQueue.Prio.Max
    import Logger
    import Solver.Distance
    import Solver.Grid

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type NextNodesFunc = [Int] -> Distance -> MaxPQueue Int Int -> MaxPQueue Int Int

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    instance Read SearchType where
        readsPrec _ "astar"     = [(Astar, "astar")]
        readsPrec _ "uniform"   = [(Uniform, "uniform")]
        readsPrec _ "greedy"    = [(Greedy, "greedy")]

    astar :: [Int] -> MaxPQueue Int Int -> Distance -> NextNodesFunc -> IO ()
    astar xs pq d nn
        | xs == getSolvedGrid (getPuzzleSize xs) = displayGrid xs
        | otherwise = do
            displayGrid xs
            let nxt = nn xs d pq
            let xs' = swapValues (snd $ findMax nxt) 0 xs
            astar xs' (deleteMax nxt) d nn
