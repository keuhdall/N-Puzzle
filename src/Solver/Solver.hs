module Solver.Solver where
    import Data.Sort
    import Solver.BTree
    import Solver.Methods

    -- Returns a solved grid of the given size
    getSolvedGrid :: Int -> [Int]
    getSolvedGrid n = let xs = replicate (n^2) (-1) in getSolvedGrid' xs 1 0 1 0 0 where
        getSolvedGrid' :: [Int] -> Int -> Int -> Int -> Int -> Int -> [Int]
        getSolvedGrid' xs' cur x ix y iy
            | cur == n^2 = replace xs' 0 (x+y*n)
            | (x + ix == n) || (x + ix < 0) || (ix /= 0 && (xs' !! (x+ix+y*n)) /= (-1)) = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) x 0 (y+ix) ix
            | (y + iy == n) || (y + iy < 0) || (iy /= 0 && (xs' !! (x+(y+iy)*n)) /= (-1)) = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) (x-iy) (-iy) y 0
            | otherwise = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) (x+ix) ix (y+iy) iy
        replace :: [a] -> a -> Int -> [a]
        replace [] _ _ = []
        replace (x:xs) x' n
            | n == 0    = x':xs
            | otherwise = x:replace xs x' (n-1)

    getPuzzleSize :: [a] -> Int
    getPuzzleSize xs = floor . sqrt . fromIntegral $ length xs

    isSolved :: [Int] -> Bool
    isSolved xs = let size = getPuzzleSize xs in xs == getSolvedGrid size

    -- Returns the given list with indexes assotiated to each value
    getIndexes :: [Int] -> [(Int, Int)]
    getIndexes xs = let len = (length xs) - 1 in zip xs [0..len]

    -- Returns the coordinates of the given value in the list
    getCoordinates :: [Int] -> Int -> (Int, Int)
    getCoordinates xs n =
        let size = getPuzzleSize xs;
            x' = snd . head $ filter (\x -> fst x == n) $ getIndexes xs;
            x = x' `mod` size;
            y = x' `div` size in (x, y)

    -- Returns the value associated to the given coordinates in the puzzle
    fromCoordinates :: [Int] -> (Int, Int) -> Int
    fromCoordinates xs (a,b) = let size = getPuzzleSize xs in xs !! (size*b+a)

    -- Returns a list of coordinates which are the coordinates of the neighbors of the `0` value in the puzzle
    getNeighbors :: [Int] -> [(Int, Int)]
    getNeighbors xs = let max = (getPuzzleSize xs)-1 in case (getCoordinates xs 0) of
        (0,0)                         -> [(0,1),(1,0)]
        (a,b) | a == max && b == max  -> [(max-1,max),(max,max-1)]
        (0,b) | b == max              -> [(0,b-1),(1,b)]
        (a,0) | a == max              -> [(a,1),(a-1,0)]
        (a,b) | b == max              -> [(a,b-1),(a-1,b),(a+1,b)]
        (a,b) | a == max              -> [(a-1,b),(a,b-1),(a,b+1)]
        (a,0)                         -> [(a,1),(a-1,0),(a+1,0)]
        (0,b)                         -> [(1,b),(0,b-1),(0,b+1)]
        (a,b)                         -> [(a,b-1),(a,b+1),(a-1,b),(a+1,b)]

    --solve :: [Int] -> (Tree -> ((Int, Int) -> (Int, Int) -> Int) -> [Int] -> [Int] -> Tree) -> Tree