module Checker (isSolvable) where

    getInvCount :: [Int] -> Int
    getInvCount []     = 0
    getInvCount (x:xs) = sum [(if x /= 0 && y /= 0 && x > y then 1 else 0) | y <- xs] + getInvCount xs

    isSolvable :: [Int] -> Bool
    isSolvable xs = even $ getInvCount xs