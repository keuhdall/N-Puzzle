module Solver.Methods where
    import qualified Data.PQueue.Prio.Max as PQ
    import Logger
    import Solver.Distance
    import Solver.Grid

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type NextNodesFunc = [Int] -> Distance -> PQ.MaxPQueue Int Int -> PQ.MaxPQueue Int Int

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    instance Read SearchType where
        readsPrec _ "astar"     = [(Astar, "astar")]
        readsPrec _ "uniform"   = [(Uniform, "uniform")]
        readsPrec _ "greedy"    = [(Greedy, "greedy")]

    astar :: [Int] -> PQ.MaxPQueue Int Int -> [[Int]] -> Distance -> NextNodesFunc -> IO ()
    astar xs os cs d nn
        | xs == getSolvedGrid (getPuzzleSize xs) = displayGrid xs
        | filter (==xs) cs /= [] = astar (xs' os) (PQ.deleteMax os) cs d nn
        | otherwise = let nxt = nn xs d os in displayGrid xs >> (astar (xs' nxt) nxt (xs:cs) d nn) where
            xs' a = swapValues (snd $ PQ.findMax a) 0 xs