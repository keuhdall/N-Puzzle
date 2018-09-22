module Reader.Reader () where
    import Reader.Data
    
    import qualified Data.ByteString.Lazy as B
    fget name = B.readFile name

    readData :: IO [String] -> IO ()
    readData args =
        | ((length args) < 1) = putStrLn $ "Please provide a filename as argument"
        | otherwise =
            content <- fget (args !! 0)
            case (checkFile content) of
                Nothing     -> putStrLn $ "Error : Invalid file format"
                Just puzzle -> do
                    solvePuzzle puzzle
                    ()