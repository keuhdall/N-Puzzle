module Utils where
    chunkList :: [Int] -> Int -> [[Int]]
    chunkList' [] n = []
    chunkList' xs n = (take n xs) : (chunkList' n (drop n xs))