module Logger where
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

    putErr :: Error -> IO ()
    putErr e = putStrLn $ show e