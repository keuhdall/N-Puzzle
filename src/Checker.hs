module Checker (isSolvable) where

    -- Counts inversions for a given N-Puzzle
    getInvCount :: [Int] -> Int
    getInvCount []     = 0
    getInvCount (x:xs) = sum [(if x /= 0 && y /= 0 && x > y then 1 else 0) | y <- xs] + getInvCount xs

    -- If the total number of inversions is even, then the Puzzle is solvable, otherwise it's not
    isSolvable :: [Int] -> Bool
    isSolvable xs = even $ getInvCount xs