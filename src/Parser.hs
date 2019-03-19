module Parser where
    import Data.Char
    import Data.List.Split

    maxPuzzle :: Int
    maxPuzzle = 10

    -- Strips comments and empty lines
    clearInput :: [String] -> [String]
    clearInput xs = filter (/="") $ map (\x -> (splitOn "#" x) !! 0) xs

    isValidSize :: [[Int]] -> Bool
    isValidSize xss = let ys = map (\y -> y^2) [3..maxPuzzle] in length (concat xss) `elem` ys

    transformInput :: [[String]] -> Maybe [[Int]]
    transformInput xs
        | all isDigit (concat (concat xs)) == False = Nothing
        | otherwise = let xss = map (\x -> map (\y -> read y) x) xs in if isValidSize xss then Just (xss) else Nothing