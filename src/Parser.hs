module Parser (clearInput, transformInput) where
    import Data.Char
    import Data.Sort
    import Data.List.Split

    maxPuzzle :: Int
    maxPuzzle = 10

    -- Strips comments and empty lines
    clearInput :: [String] -> [String]
    clearInput xs = filter (/="") $ map (\x -> (splitOn "#" x) !! 0) xs

    -- Checking size of the puzzle
    isValidSize :: [[Int]] -> Bool
    isValidSize xss = let ys = map (\y -> y^2) [3..maxPuzzle] in (length . concat) xss `elem` ys

    -- Sorts input and compare it to an enum list of the same size
    hasValidContent :: [[Int]] -> Bool
    hasValidContent xss = let n = (length xs) - 1; xs = concat xss; ys = [0..n] in (sort xs) == ys

    -- Returns [[Int]] if the input is valid, otherwise returns Nothing
    transformInput :: [[String]] -> Maybe [[Int]]
    transformInput xss
        | all isDigit (concat $ concat xss) == False = Nothing
        | otherwise = let xss' = map (\xs -> map (\x -> read x) xs) xss in if (isValidSize xss' && hasValidContent xss') then Just (xss') else Nothing
