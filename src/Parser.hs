module Parser (clearInput, transformInput) where
    import Data.Char
    import Data.Sort
    import Data.List.Split

    import Solver.Distance
    import Solver.Solver

    maxPuzzle :: Int
    maxPuzzle = 10

    -- Strips comments and empty lines
    clearInput :: [String] -> [String]
    clearInput xs = filter (/="") $ map (\x -> (splitOn "#" x) !! 0) xs

    -- Checking size of the puzzle
    isValidSize :: [[Int]] -> Bool
    isValidSize xss = let ys = map (^2) [3..maxPuzzle] in (length . concat) xss `elem` ys

    -- Sorts input and compare it to an enum list of the same size
    hasValidContent :: [[Int]] -> Bool
    hasValidContent xss = let n = (length xs) - 1; xs = concat xss; ys = [0..n] in (sort xs) == ys

    -- Returns the input as [[Int]] if it is valid, otherwise returns Nothing
    transformInput :: [[String]] -> Maybe [[Int]]
    transformInput xss
        | all isDigit (concat $ concat xss) == False = Nothing
        | otherwise = let xss' = map (\xs -> map read xs) xss in if (isValidSize xss' && hasValidContent xss') then Just (xss') else Nothing

    -- Returns a SearchType or a SearchType and a Distance if they have been provided as arguments
    handleArgs :: [String] -> (Maybe SearchType, Maybe Distance)
    handleArgs xs = case (length xs) of
        2       -> (Just (read $ xs !! 1), Nothing)
        3       -> (Just (read $ xs !! 1), Just (read $ xs !! 2))
        _       -> (Nothing, Nothing)