module Solver.Solver (SearchType(..), solve) where
    import Data.Sort
    import qualified Data.PQueue.Prio.Max as PQ
    import qualified Error as E
    import Logger
    import Solver.Grid
    import Solver.Distance

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type DistFunc = (Int, Int) -> (Int, Int) -> Int
    type NextNodesFunc = SearchType -> [[Int]] -> [[Int]] -> PQ.MaxPQueue Int Int

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    -- Returns the cost of a node according to the SearchType currently used
    getCost :: SearchType -> DistFunc -> [[Int]] -> (Int, Int) -> Int
    getCost st d xss coord =
        let xs    =  head xss
            svd   =  getSolvedGrid $ getPuzzleSize xs
            val   =  fromCoordinates xs coord
            dist  =  d (getCoordinates svd val) coord in case st of
            Astar     -> dist + (length xss)    -- A* : h cost + g cost
            Uniform   -> (length xss)           -- Uniform : g cost only
            Greedy    -> dist                   -- Greedy : h cost only

    -- Returns a PQueue containing the next nodes (value + cost)
    getNextNodes :: Distance -> SearchType -> [[Int]] -> [[Int]] -> PQ.MaxPQueue Int Int
    getNextNodes d st xss cs = getNextNodes' (getNeighbors xs) PQ.empty where
        xs    = head xss
        svd   = getSolvedGrid $ getPuzzleSize xs
        dist  = getDistance d
        cost  = getCost st dist xss
        value = fromCoordinates xs
        getNextNodes' (y:[]) pq = PQ.filter (\x -> (swapValues x xs) `notElem` cs) $ PQ.insert (cost y) (value y) pq
        getNextNodes' (y:ys) pq = getNextNodes' ys (PQ.insert (cost y) (value y) pq)

    runSearch :: [[Int]] -> PQ.MaxPQueue Int Int -> [[Int]] -> SearchType -> NextNodesFunc -> Int -> IO ()
    runSearch xss os cs st nn n
        | xs == getSolvedGrid (getPuzzleSize xs) = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | cln == PQ.empty && os /= PQ.empty = displayGrid xs >> runSearch (tail xss) os (xs:cs) st nn (n+1)
        | cln == PQ.empty = putErr E.NotSolvable
        | otherwise = displayGrid xs >> runSearch ((swapValues (snd max) xs):xss) os' (xs:cs) st nn (n+1) where
                xs  = head xss
                gn  = map (fromCoordinates xs) $ getNeighbors xs
                cln = PQ.filter ((flip elem) gn) $ PQ.union (nn st xss cs) os
                max = PQ.findMax cln
                os' = PQ.filterWithKey (\x y -> (x /= (fst max) && y /= (snd max))) $ PQ.union (nn st xss cs) os

    solve :: [Int] -> SearchType -> Distance -> IO ()
    solve xs st d = let nn = getNextNodes d in runSearch [xs] PQ.empty [] st nn 0