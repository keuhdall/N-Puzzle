module Solver.Solver where
    import Data.Sort
    import PQueue.PQueue
    import Solver.Grid
    import Solver.Methods
    import Solver.Distance

    getNextNodes :: [Int] -> Distance -> PQueue Int
    getNextNodes xs d = getNextNodes' (getNeighbors xs) (getSolvedGrid $ getPuzzleSize xs) (getDistance d) [] where
        getNextNodes' (y:[]) svd dist pq = insert (Item (fromCoordinates xs y) (dist (getCoordinates svd $ fromCoordinates xs y) y)) pq
        getNextNodes' (y:ys) svd dist pq = getNextNodes' ys svd dist (insert (Item (fromCoordinates xs y) (dist (getCoordinates svd $ fromCoordinates xs y) y)) pq)
        

    -- solve :: [Int] -> SearchType -> Distance -> IO ()