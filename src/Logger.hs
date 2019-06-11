module Logger (Error(..), displayHelp, displayGrid, putErr) where
    import Grid
    import Text.Printf (printf)

    data Error = NotSolvable | InvalidInput

    instance Show Error where
        show NotSolvable        = "Error: puzzle is not solvable."
        show InvalidInput       = "Error: input is invalid."

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
        printElems [] _ = putStrLn $ replicate ((size * (align + 1)) - 1) '-'
        printElems (x:xs) n = printf (if n `mod` size == 0 then "%*d\n" else "%*d ") align x >> printElems xs (n+1)

    putErr :: Error -> IO ()
    putErr e = putStrLn $ show e
