module Logger where
    import Error

    help :: IO ()
    help = putStrLn $ "Usage : cabal run [filename] (method) (heuristic)\n \
    \ Supported metods :\n \
    \ - astar \n \
    \ - uniform \n \
    \ - greedy \n \
    \ Supported heuristics : \n \
    \ - manhattan \n \ 
    \ - diagonal \n \
    \ - euclidian \n \
    \ - hamming \n"

    putErr :: Error -> IO ()
    putErr e = putStrLn $ show e