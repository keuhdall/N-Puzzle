module Solver.Solver where
    import Data.Sort
    import Data.PQueue.Prio.Max
    import Solver.Grid
    import Solver.Methods
    import Solver.Distance

    getNextNodes :: [Int] -> Distance -> MaxPQueue Int Int -> MaxPQueue Int Int
    getNextNodes xs d pq = getNextNodes' (getNeighbors xs) pq where
        svd = getSolvedGrid $ getPuzzleSize xs
        dist = getDistance d
        getNextNodes' (y:[]) pq = insert (dist (getCoordinates svd $ fromCoordinates xs y) y) (fromCoordinates xs y) pq
        getNextNodes' (y:ys) pq = getNextNodes' ys (insert (fromCoordinates xs y) (dist (getCoordinates svd $ fromCoordinates xs y) y) pq)
        

    solve :: [Int] -> SearchType -> Distance -> IO ()
    solve xs st d = astar xs empty d getNextNodes