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

    displayGrid :: Grid -> IO ()
    displayGrid grid = printElems (concat grid) 1 where
        size  = getPuzzleSize grid
        align = maximum $ (length . show) <$> (concat grid)
        printElems xs@(x:xs') n
            | xs == []  = putStrLn $ replicate ((size * (align + 1)) - 1) '-'
            | otherwise = printf (if n `mod` size == 0 then "%*d\n" else "%*d") align x >> printElems xs' (n+1)

    putErr :: Error -> IO ()
    putErr e = putStrLn $ show e