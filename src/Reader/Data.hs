module Reader.Data (Puzzle(..)) where
    data Puzzle = Puzzle {
        data::[String]
    } deriving (Show)