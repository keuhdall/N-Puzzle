module Solver.Distance where
    
    manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
    manhattanDistance x y = abs (fst x - fst y) + abs (snd x - snd y)

    diagonalDistance :: (Int, Int) -> (Int, Int) -> Int
    diagonalDistance x y = max $ abs (fst x - fst y) abs (snd x - snd y)

    euclidianDistance :: (Int, Int) -> (Int, Int) -> Int
    euclidianDistance x y = sqrt $ (fst x - fst y)*2 + (snd x - snd y)*2

    