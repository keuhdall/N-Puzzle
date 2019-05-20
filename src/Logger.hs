module Logger (displayHelp, displayGrid, putErr) where
    import Solver.Grid
    import Error

    displayHelp :: IO ()
    displayHelp = putStrLn $ "Usage : cabal run [filename] (method) (heuristic)\n \
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
    displayGrid grid = let size = getPuzzleSize grid in mapM_ (\xs -> putStrLn . concat . map (++" ") $ map show xs) grid >> (putStrLn $ replicate (2 * size - 1) '-')

    putErr :: Error -> IO ()
    putErr e = putStrLn $ show e