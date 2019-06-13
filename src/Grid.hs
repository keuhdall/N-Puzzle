module Grid (Grid, getSolvedGrid, chunkList, getPuzzleSize, getCoordinates, getNeighbors) where
    import Prelude hiding (Left, Right)
    import Data.Maybe

    data Move = Up | Down | Left | Right deriving Eq
    type Grid = [[Int]]

    -- Returns a solved grid of the given size
    getSolvedGrid :: Int -> Grid
    getSolvedGrid n = let xs = replicate (n^2) (-1) in chunkList n $ getSolvedGrid' xs 1 0 1 0 0 where
        getSolvedGrid' xs' cur x ix y iy
            | cur == n^2 = replace xs' 0 (x+y*n)
            | x + ix == n || x + ix < 0 || ix /= 0 && xs' !! (x+ix+y*n)   /= (-1)   = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) x      0     (y+ix) ix
            | y + iy == n || y + iy < 0 || iy /= 0 && xs' !! (x+(y+iy)*n) /= (-1)   = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) (x-iy) (-iy) y      0
            | otherwise = getSolvedGrid' (replace xs' cur $ x+y*n) (cur+1) (x+ix) ix (y+iy) iy
        replace [] _ _ = []
        replace (_:xs) x' 0 = x':xs
        replace (x:xs) x' m = x:replace xs x' (m-1)

    chunkList :: Int -> [Int] -> Grid
    chunkList _ [] = []
    chunkList n xs = (take n xs) : (chunkList n (drop n xs))

    getPuzzleSize :: Grid -> Int
    getPuzzleSize grid = length $ head grid

    -- Returns the value associated to the given coordinates in the puzzle
    fromCoordinates :: Grid -> (Int, Int) -> Int
    fromCoordinates grid (x,y) = ((grid !! y) !! x)

    getCoordinates :: Grid -> Int -> (Int, Int)
    getCoordinates grid n = let size = getPuzzleSize grid - 1 in head $ filter(/=(-1,-1)) [if ((grid !! y) !! x) == n then (x,y) else (-1,-1) | x <- [0..size], y <- [0..size]]

    -- Returns the coordinates of the zero in a given grid
    getZero :: Grid -> (Int, Int)
    getZero grid = getCoordinates grid 0

    -- Returns a list of coordinates which are the coordinates of the neighbors of the `0` value in the puzzle
    getNeighbors :: Grid -> [Grid]
    getNeighbors grid = map fromJust $ filter (/= Nothing) $ (updateGrid grid) <$> [Up, Down, Left, Right]

    -- Get the new coordinates of the zero value
    moveZero :: Move -> (Int, Int) -> (Int, Int)
    moveZero move (x,y) = case move of
        Up      -> (x,y-1)
        Down    -> (x,y+1)
        Left    -> (x-1,y)
        Right   -> (x+1,y)

    -- Tries to update a grid with a given move, returns Just Grid if it succeeds, Nothing if it fails
    updateGrid :: Grid -> Move -> Maybe Grid
    updateGrid grid move = if checkPos newPos == False then Nothing else Just $ swapValues grid pos newPos where
        size = getPuzzleSize grid - 1
        pos = getZero grid
        newPos = moveZero move pos
        checkPos (x, y) = if x < 0 || x > size || y < 0 || y > size then False else True

    -- Swap 2 values from a grid
    swapValues :: Grid -> (Int, Int) -> (Int, Int) -> Grid
    swapValues grid x y = let size = getPuzzleSize grid in chunkList size $ map (\z -> if z == a then b else if z == b then a else z) (concat grid) where
        a = fromCoordinates grid x
        b = fromCoordinates grid y