module Parser.Parser where
    isComment :: String -> Bool
    isComment s
        | (s !! 0) == '#'   = True
        | otherwise         = False


    stripComment :: String -> String
    stripComment s
        | (length (stripComment' s) == 0)   = []
        | (length (stripComment' s) == 1)   = (stripComment' s !! 0)
        | otherwise                         = (stripComment' s !! 1)
        where
            stripComment' :: String -> [String]
            stripComment' s = 
                let (start, end) = break (=='#') s in
                start : if null end then [] else stripComment' $ tail end