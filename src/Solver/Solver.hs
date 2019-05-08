module Solver.Solver (SearchType(..), solve) where
    import Data.Sort
    import qualified Data.PQueue.Prio.Max as PQ
    import qualified Error as E
    import Logger
    import Solver.Grid
    import Solver.Distance

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type DistFunc = (Int, Int) -> (Int, Int) -> Int

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    -- Function that returns the cost of a node according to the SearchType currently used
    getCost :: SearchType -> DistFunc -> [[Int]] -> (Int, Int) -> Int
    getCost st d xss coord =
        let xs    =  head xss
            svd   =  getSolvedGrid $ getPuzzleSize xs
            val   =  fromCoordinates xs coord
            dist  =  d (getCoordinates svd val) coord in case st of
            Astar     -> dist + (length xss)    -- A* : h cost + g cost
            Uniform   -> (length xss)           -- Uniform : g cost only
            Greedy    -> dist                   -- Greedy : h cost only

    -- Function that returns a PQueue containing the next nodes (value + cost)
    getNextNodes :: [[Int]] -> [Int] -> SearchType -> Distance -> PQ.MaxPQueue Int Int -> PQ.MaxPQueue Int Int
    getNextNodes xss xs st d pq = getNextNodes' (getNeighbors xs) pq where
        svd   = getSolvedGrid $ getPuzzleSize xs
        dist  = getDistance d
        cost  = getCost st dist xss
        value = fromCoordinates xs
        getNextNodes' (y:[]) pq = PQ.insert (cost y) (value y) pq
        getNextNodes' (y:ys) pq = getNextNodes' ys (PQ.insert (cost y) (value y) pq)

    runSearch :: [[Int]] -> PQ.MaxPQueue Int Int -> [[Int]] -> SearchType -> Distance -> Int -> IO ()
    runSearch xss os cs st d n
        | xs == getSolvedGrid (getPuzzleSize xs) = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | cln == PQ.empty && os /= PQ.empty = displayGrid xs >> runSearch (tail xss) os (xs:cs) st d (n+1)
        | cln == PQ.empty = putErr E.NotSolvable
        | otherwise = displayGrid xs >> (runSearch ((swp cln):xss) nxt (xs:cs) st d (n+1)) where
            xs      =  head xss
            swp x   =  swapValues (snd $ PQ.findMax x) 0 xs
            gn xs   =  map (fromCoordinates xs) $ getNeighbors xs
            cln     =  PQ.filter (\x -> ((swapValues x 0 xs) `notElem` cs) && (x `elem` (gn xs))) os
            nxt     =  getNextNodes xss (swp cln) st d os

    solve :: [Int] -> SearchType -> Distance -> IO ()
    solve xs st d = runSearch [xs] (getNextNodes [xs] xs st d PQ.empty) [] st d 0