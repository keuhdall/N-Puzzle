module Solver.Solver where
    import Data.Sort
    import PQueue.PQueue
    import Solver.Grid
    import Solver.Methods
    import Solver.Distance

    getNextNodes :: [Int] -> Distance -> PQueue Int
    getNextNodes xs d = getNextNodes' (getNeighbors xs) [] where
        svd = getSolvedGrid $ getPuzzleSize xs
        dist = getDistance d
        getNextNodes' (y:[]) pq = insert (Item (fromCoordinates xs y) (dist (getCoordinates svd $ fromCoordinates xs y) y)) pq
        getNextNodes' (y:ys) pq = getNextNodes' ys (insert (Item (fromCoordinates xs y) (dist (getCoordinates svd $ fromCoordinates xs y) y)) pq)
        

    -- solve :: [Int] -> SearchType -> Distance -> IO ()