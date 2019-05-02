module Solver.Methods where
    import qualified Data.PQueue.Prio.Max as PQ
    import qualified Error as E
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

    astar :: [[Int]] -> PQ.MaxPQueue Int Int -> [[Int]] -> Distance -> NextNodesFunc -> Int -> IO ()
    astar xss os cs d nn n
        | xs == getSolvedGrid (getPuzzleSize xs) = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | cln == PQ.empty && os /= PQ.empty = astar (tail xss) os (xs:cs) d nn (n+1)
        | cln == PQ.empty = putErr E.NotSolvable
        | otherwise = displayGrid xs >> (astar ((swp cln):xss) nxt (xs:cs) d nn) (n+1) where
            xs      =  head xss
            swp x   =  swapValues (snd $ PQ.findMax x) 0 xs
            gn xs   =  map (fromCoordinates xs) $ getNeighbors xs
            cln     =  PQ.filter (\x -> ((swapValues x 0 xs) `notElem` cs) && (x `elem` (gn xs))) os
            nxt     =  nn (swp cln) d cln