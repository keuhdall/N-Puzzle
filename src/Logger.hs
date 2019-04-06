module Logger where
    import Error

    help :: IO ()
    help = putStrLn $ "Usage : cabal run [filename] (method) (heuristic)"

    putErr :: Error -> IO ()
    putErr NotSolvable    = putStrLn $ "Error : puzzle is not solvable."
    putErr InvalidInput   = putStrLn $ "Error : input is invalid."