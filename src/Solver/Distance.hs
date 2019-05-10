module Solver.Distance (Distance(..), readDistance, getDistance) where

    data Distance = Manhattan | Diagonal | Euclidian | Hamming deriving (Show, Eq)

    manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
    manhattanDistance x y = abs (fst x - fst y) + abs (snd x - snd y)

    diagonalDistance :: (Int, Int) -> (Int, Int) -> Int
    diagonalDistance x y = let x' = (abs $ fst x - fst y); y' = (abs $ snd x - snd y) in max x' y'

    euclidianDistance :: (Int, Int) -> (Int, Int) -> Int
    euclidianDistance x y = floor $ sqrt $ fromIntegral $ (fst x - fst y)*2 + (snd x - snd y)*2

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