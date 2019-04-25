module Solver.Distance (Distance(..), getDistance) where

    data Distance = Manhattan | Diagonal | Euclidian | Hamming deriving (Show, Eq)

    instance Read Distance where
        readsPrec _ "manhattan" = [(Manhattan, "manhattan")]
        readsPrec _ "diagonal"  = [(Diagonal, "diagonal")]
        readsPrec _ "euclidian" = [(Euclidian, "euclidian")]
        readsPrec _ "hamming"   = [(Hamming, "hamming")]

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