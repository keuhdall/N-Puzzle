module Logger (displayHelp, displayGrid, putErr) where
    import Solver.Grid
    import Error
    import Text.Printf (printf)

    displayHelp :: IO ()
    displayHelp = putStrLn $ "Usage : N-Puzzle [filename] (method) (heuristic)\n \
    \ \n \
    \ Supported metods :\n \
    \ - astar \n \
    \ - uniform \n \
    \ - greedy \n \
    \ \n \
    \ Supported heuristics : \n \
    \ - manhattan \n \
    \ - diagonal \n \
    \ - euclidian \n \
    \ - hamming \n"

    -- displayGrid :: Grid -> IO ()
    -- displayGrid grid = let size = getPuzzleSize grid in mapM_ (\xs -> putStrLn . concat . map (++" ") $ map show xs) grid >> (putStrLn $ replicate (2 * size - 1) '-')

    displayGrid :: Grid -> IO ()
    displayGrid grid =
        printElems elems (getPuzzleSize grid) 1 (maximum (map (length . show) elems))
        where
            elems = concat grid
            printElems :: [Int] -> Int -> Int -> Int -> IO ()
            printElems list size n alignment
                | list == [] = putStrLn $ replicate ((size * (alignment + 1)) - 1) '-'
                | n `mod` size == 0 = printf "%*d\n" alignment (head list) >> printElems (tail list) size (n + 1) alignment
                | otherwise = printf "%*d " alignment (head list) >> printElems (tail list) size (n + 1) alignment

    putErr :: Error -> IO ()
    putErr e = putStrLn $ show e