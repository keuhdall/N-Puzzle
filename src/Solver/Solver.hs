module Solver.Solver where
    import Data.Sort
    import qualified Data.PQueue.Prio.Max as PQ
    import Solver.Grid
    import Solver.Methods
    import Solver.Distance

    getNextNodes :: [Int] -> Distance -> PQ.MaxPQueue Int Int
    getNextNodes xs d = getNextNodes' (getNeighbors xs) PQ.empty where
        svd = getSolvedGrid $ getPuzzleSize xs
        dist = getDistance d
        getNextNodes' (y:[]) pq = PQ.insert (dist (getCoordinates svd $ fromCoordinates xs y) y) (fromCoordinates xs y) pq
        getNextNodes' (y:ys) pq = getNextNodes' ys (PQ.insert (dist (getCoordinates svd $ fromCoordinates xs y) y) (fromCoordinates xs y) pq)

    solve :: [Int] -> SearchType -> Distance -> IO ()
    solve xs st d = astar xs (getNextNodes xs d) [] d getNextNodes