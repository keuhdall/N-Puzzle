module Solver.Grid where
    import Prelude hiding (Left, Right)
    import Data.Maybe
    import Data.List
    import Data.Array

    data Move = Up | Down | Left | Right deriving Eq
    type Grid = Array Int (Int, Int)

    -- Returns a solved grid of the given size
    getSolvedGrid :: Int -> [Int] -- ok
    getSolvedGrid n = let xs = replicate (n^2) (-1) in getSolvedGrid' xs 1 0 1 0 0 where
        getSolvedGrid' xs' cur x ix y iy
            | cur == n^2 = replace xs' 0 (x+y*n)
            | (x + ix == n) || (x + ix < 0) || (ix /= 0 && (xs' !! (x+ix+y*n)) /= (-1))     = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) x 0 (y+ix) ix
            | (y + iy == n) || (y + iy < 0) || (iy /= 0 && (xs' !! (x+(y+iy)*n)) /= (-1))   = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) (x-iy) (-iy) y 0
            | otherwise = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) (x+ix) ix (y+iy) iy
        replace [] _ _ = []
        replace (x:xs) x' n
            | n == 0    = x':xs
            | otherwise = x:replace xs x' (n-1)

    getPuzzleSize :: Grid -> Int -- ok
    getPuzzleSize grid = floor . sqrt . fromIntegral $ length grid

    isSolved :: Grid -> Bool -- ok
    isSolved grid = let size = getPuzzleSize grid; xs = map fst $ assocs grid in xs == getSolvedGrid size

    -- Returns a list of coordinates which are the coordinates of the neighbors of the `0` value in the puzzle
    getNeighbors :: Grid -> [Grid]
    getNeighbors grid = map fromJust $ filter (/= Nothing) $ [Up, Down, Left, Right] >>= (\x -> [updateGrid grid x])

    -- Swap 2 values from a list
    updateGrid :: Grid -> Move -> Maybe Grid
    updateGrid grid pos = if null ind then Nothing else Just $ grid//[(0, newCoord), ((fst $ head ind), grid!0)] where
        size = getPuzzleSize grid - 1
        newCoord = moveTile (grid!0) pos
        ind = filter (\x -> snd x == newCoord) $ assocs grid

    moveTile :: (Int, Int) -> Move -> (Int, Int)
    moveTile (x, y) pos = case pos of
        Up      -> (x, y-1)
        Down    -> (x, y+1)
        Left    -> (x-1, y)
        Right   -> (x+1, y)