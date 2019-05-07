module Solver.Solver (SearchType(..), getSearchType, solve) where
    import Data.Sort
    import qualified Data.PQueue.Prio.Max as PQ
    import qualified Error as E
    import Logger
    import Solver.Grid
    import Solver.Distance

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type SeachFunc = [[Int]] -> PQ.MaxPQueue Int Int -> [[Int]] -> Distance -> Int -> IO ()
    type DistFunc = (Int, Int) -> (Int, Int) -> Int

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    instance Read SearchType where
        readsPrec _ "astar"     = [(Astar, "astar")]
        readsPrec _ "uniform"   = [(Uniform, "uniform")]
        readsPrec _ "greedy"    = [(Greedy, "greedy")]

    getSearchType :: SearchType -> SeachFunc
    getSearchType st = case st of
        Astar   -> astar
        Greedy  -> greedy

    getCost :: SearchType -> DistFunc -> [[Int]] -> (Int, Int) -> Int
    getCost st d xss coord =
        let xs    =  head xss
            svd   =  getSolvedGrid $ getPuzzleSize xs
            val   =  fromCoordinates xs coord
            dist  =  d (getCoordinates svd val) coord in case st of
            Astar     -> dist + (length xss)
            Uniform   -> (length xss)
            Greedy    -> dist

    getNextNodes :: [[Int]] -> [Int] -> SearchType -> Distance -> PQ.MaxPQueue Int Int -> PQ.MaxPQueue Int Int
    getNextNodes xss xs st d pq = getNextNodes' (getNeighbors xs) pq where
        svd   = getSolvedGrid $ getPuzzleSize xs
        dist  = getDistance d
        cost  = getCost st dist xss
        value = fromCoordinates xs
        getNextNodes' (y:[]) pq = PQ.insert (cost y) (value y) pq
        getNextNodes' (y:ys) pq = getNextNodes' ys (PQ.insert (cost y) (value y) pq)

    astar :: [[Int]] -> PQ.MaxPQueue Int Int -> [[Int]] -> Distance -> Int -> IO ()
    astar xss os cs d n
        | xs == getSolvedGrid (getPuzzleSize xs) = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | cln == PQ.empty && os /= PQ.empty = astar (tail xss) os (xs:cs) d (n+1)
        | cln == PQ.empty = putErr E.NotSolvable
        | otherwise = displayGrid xs >> (astar ((swp cln):xss) nxt (xs:cs) d (n+1)) where
            xs      =  head xss
            swp x   =  swapValues (snd $ PQ.findMax x) 0 xs
            gn xs   =  map (fromCoordinates xs) $ getNeighbors xs
            cln     =  PQ.filter (\x -> ((swapValues x 0 xs) `notElem` cs) && (x `elem` (gn xs))) os
            nxt     =  getNextNodes xss (swp cln) Astar d os

    greedy :: [[Int]] -> PQ.MaxPQueue Int Int -> [[Int]] -> Distance -> Int -> IO ()
    greedy xss os cs d n
        | xs == getSolvedGrid (getPuzzleSize xs) = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | cln == PQ.empty && os /= PQ.empty = greedy (tail xss) os (xs:cs) d (n+1)
        | cln == PQ.empty = putErr E.NotSolvable
        | otherwise = displayGrid xs >> (greedy ((swp cln):xss) nxt (xs:cs) d (n+1)) where
            xs      =  head xss
            swp x   =  swapValues (snd $ PQ.findMax x) 0 xs
            gn xs   =  map (fromCoordinates xs) $ getNeighbors xs
            cln     =  PQ.filter (\x -> ((swapValues x 0 xs) `notElem` cs) && (x `elem` (gn xs))) os
            nxt     =  getNextNodes xss (swp cln) Greedy d os

    uniform :: [[Int]] -> PQ.MaxPQueue Int Int -> [[Int]] -> Distance -> Int -> IO ()
    uniform xss os cs d n
        | xs == getSolvedGrid (getPuzzleSize xs) = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | cln == PQ.empty && os /= PQ.empty = greedy (tail xss) os (xs:cs) d (n+1)
        | cln == PQ.empty = putErr E.NotSolvable
        | otherwise = displayGrid xs >> (greedy ((swp cln):xss) nxt (xs:cs) d (n+1)) where
            xs      =  head xss
            swp x   =  swapValues (snd $ PQ.findMax x) 0 xs
            gn xs   =  map (fromCoordinates xs) $ getNeighbors xs
            cln     =  PQ.filter (\x -> ((swapValues x 0 xs) `notElem` cs) && (x `elem` (gn xs))) os
            nxt     =  getNextNodes xss (swp cln) Uniform d os
        

    solve :: [Int] -> SearchType -> Distance -> IO ()
    solve xs st d = astar [xs] (getNextNodes [xs] xs st d PQ.empty) [] d 0