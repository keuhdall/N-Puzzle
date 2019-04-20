module Solver.Solver where
    import Data.Sort
    import Solver.Grid
    import Solver.Methods
    import Solver.Distance

    getNextNodes :: [Int] -> Distance -> [(Int, Int)]
    getNextNodes xs d = let size = getPuzzleSize xs; ys = getSolvedGrid size; xs' = getNeighbors xs; dist = getDistance d in
        map (\x -> ((fromCoordinates xs x), (dist (getCoordinates ys $ fromCoordinates xs x) x))) xs'

    --solve :: [Int] -> (Tree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> Tree) -> Tree