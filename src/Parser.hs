module Parser (clearInput, transformInput, parseArgs) where
    import Data.Char
    import Data.Sort
    import Data.List.Split
    import Text.Read

    import Solver.Distance
    import Solver.Solver

    maxPuzzle :: Int
    maxPuzzle = 5

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

    parseArgs :: [String] -> (Maybe SearchType, Maybe Distance)
    parseArgs xs = case length xs of
        1   -> (Nothing, Nothing)
        2   -> (readSearchType (xs !! 1), Nothing)
        _   -> (readSearchType (xs !! 1), readDistance (xs !! 2)) where
