module Solver.Methods where
    import qualified Data.PQueue.Prio.Max as PQ
    import qualified Error as E
    import Logger
    import Solver.Distance
    import Solver.Grid

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type NextNodesFunc = [Int] -> Distance -> PQ.MaxPQueue Int Int

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
        | otherwise = do
            let cln = PQ.filter (\x -> (swapValues x 0 xs) `notElem` cs) os
            if (cln == PQ.empty) then putErr E.NotSolvable else displayGrid xs >> (astar (swp cln) nxt (xs:cs) d nn) where
                swp a = swapValues (snd $ PQ.findMax a) 0 xs
                nxt = nn (swp os) d