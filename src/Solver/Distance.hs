module Solver.Distance where
    manhattanDistance :: (Int, Int) -> (Int, Int) -> Float
    manhattanDistance x y = fromIntegral $ abs (fst x - fst y) + abs (snd x - snd y)

    diagonalDistance :: (Int, Int) -> (Int, Int) -> Float
    diagonalDistance x y = let x' = (abs $ fst x - fst y); y' = (abs $ snd x - snd y) in fromIntegral $ max x' y'

    euclidianDistance :: (Int, Int) -> (Int, Int) -> Float
    euclidianDistance x y = sqrt $ fromIntegral $ (fst x - fst y)*2 + (snd x - snd y)*2