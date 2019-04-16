module Solver.Solver where
    import Data.Sort
    import Solver.BTree
    import Solver.Methods

    data SearchType = Astar | Uniform | Greedy deriving Eq

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    getSearchType :: String -> Maybe SearchType
    getSearchType s = case s of
        "astar"     -> Just $ Astar
        "uniform"   -> Just $ Uniform
        "greedy"    -> Just $ Greedy
        _           -> Nothing

    toString :: SearchType -> String
    toString st = case st of
        Astar   -> "astar"
        Uniform -> "uniform"
        Greedy  -> "greedy"

    getSolvedGrid :: [Int] -> [Int]
    getSolvedGrid xs = sort xs

    getSolvedGrid :: Int -> [Int]
    getSolvedGrid n = let xs = replicate n^2 (-1) in getSolvedGrid' xs 1 0 1 0 0 where
        getSolvedGrid' xs' cur x ix y iy
            | cur == n^2 = xs
            | (x + ix == s) || (x + ix < 0) || (ix /= 0 && (xs !! (x+ix+y*n)) /= (-1)) = getSolvedGrid' xs cur+1 x 0 y+ix ix
            | (y + iy == s) || (y + iy < 0) || (iy /= 0 && (xs !! ((y+iy)*s)) /= (-1)) = getSolvedGrid' xs cur+1 x-iy (-iy) y 0

    isSolved :: [Int] -> Bool
    isSolved xs = xs == getSolvedGrid xs

    getIndexes :: [Int] -> [(Int, Int)]
    getIndexes xs = let len = (length xs) - 1 in zip xs [0..len]

    getCoordinates :: [Int] -> Int -> (Int, Int)
    getCoordinates xs n = let size = floor $ sqrt $ fromIntegral $ length xs; x = n `mod` size; y = n `div` size in (x, y)

    --solve :: [Int] -> (Tree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> Tree) -> Tree