module Solver.Distance (Distance(..), calcDistance, readDistance) where
    import Solver.Grid

    data Distance = Manhattan | Diagonal | Euclidian | Hamming deriving (Show, Eq)

    calcDistance :: Distance -> Grid -> Grid -> Int
    calcDistance d a b = calcDistance' (concat a) (concat b) 0 where
        calcDistance' [] ys n = n
        calcDistance' (x:xs) ys n = let dist = getDistance d in calcDistance' xs ys (n + sum [if x == y then dist (getCoordinates a x) (getCoordinates b y) else 0 | y <- ys])

    manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
    manhattanDistance (x,y) (x',y') = (abs $ x' - x) + (abs $ y' - y)

    diagonalDistance :: (Int, Int) -> (Int, Int) -> Int
    diagonalDistance (x,y) (x',y') = let a = (abs $ x' - x); b = (abs $ y' - y) in max a b

    euclidianDistance :: (Int, Int) -> (Int, Int) -> Int
    euclidianDistance (x,y) (x',y') = floor $ sqrt $ fromIntegral $ (x' - x)^2 + (y' - y)^2

    hammingDistance :: (Int, Int) -> (Int, Int) -> Int
    hammingDistance x y = if x == y then 1 else 0

    -- Returns the distance function assoctiated to the given algebraic type
    getDistance :: Distance -> ((Int, Int) -> (Int, Int) -> Int)
    getDistance d = case d of
        Manhattan -> manhattanDistance
        Diagonal  -> diagonalDistance
        Euclidian -> euclidianDistance
        Hamming   -> hammingDistance

    readDistance :: String -> Maybe Distance
    readDistance s = case s of
        "manhattan" -> Just Manhattan
        "diagonal"  -> Just Diagonal
        "euclidian" -> Just Euclidian
        "hamming"   -> Just Hamming
        _           -> Nothing