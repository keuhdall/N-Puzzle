module Logger where
    import Error

    help :: IO ()
    help = putStrLn $ "Usage : cabal run [filename] (method) (heuristic)"

    putErr :: Error -> IO ()
    putErr e = putStrLn $ show e