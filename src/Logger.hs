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

    chunkList :: [Int] -> [[Int]]
    chunkList xs = let size = getPuzzleSize xs in chunkList' size xs where
        chunkList' n [] = []
        chunkList' n xs = (take n xs) : (chunkList' n (drop n xs))

    displayGrid :: [Int] -> IO ()
    displayGrid xs = let xss = chunkList xs in mapM_ (\xs -> putStrLn . concat $ map show xs) xss

    putErr :: Error -> IO ()
    putErr e = putStrLn $ show e