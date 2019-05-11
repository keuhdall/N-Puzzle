module Solver.Solver (SearchType(..), readSearchType, solve) where
    import Data.Sort
    import qualified Data.PQueue.Prio.Max as PQ
    import qualified Error as E
    import Logger
    import Solver.Grid
    import Solver.Distance

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type DistFunc = (Int, Int) -> (Int, Int) -> Int
    type NextNodesFunc = SearchType -> [[Int]] -> [[Int]] -> PQ.MaxPQueue Int [[Int]]

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    readSearchType :: String -> Maybe SearchType
    readSearchType s = case s of
        "astar"     -> Just Astar
        "uniform"   -> Just Uniform
        "greedy"    -> Just Greedy
        _           -> Nothing

    -- Returns the cost of a node according to the SearchType currently used
    getCost :: SearchType -> DistFunc -> [[Int]] -> (Int, Int) -> Int
    getCost st d xss coord =
        let xs    =  head xss
            svd   =  getSolvedGrid $ getPuzzleSize xs
            val   =  fromCoordinates xs coord
            dist  =  d coord (getCoordinates svd val) in case st of
            Astar     -> dist + (length xss)    -- A* : h cost + g cost
            Uniform   -> (length xss)           -- Uniform : g cost only
            Greedy    -> dist                   -- Greedy : h cost only

    -- Returns a PQueue containing the next nodes (value + cost)
    getNextNodes :: Distance -> SearchType -> [[Int]] -> [[Int]] -> PQ.MaxPQueue Int [[Int]]
    getNextNodes d st xss cs = getNextNodes' (getNeighbors xs) PQ.empty where
        xs    = head xss
        cost  = getCost st (getDistance d) xss
        value x = (swapValues (fromCoordinates xs x) xs):xss
        getNextNodes' (y:[]) pq = PQ.filter ((flip $ notElem . head) cs) $ PQ.insert (cost y) (value y) pq
        getNextNodes' (y:ys) pq = getNextNodes' ys (PQ.insert (cost y) (value y) pq)

    -- Runs the search using a given SearchType. The SearchType will be used in nodes cost computation
    runSearch :: [[Int]] -> PQ.MaxPQueue Int [[Int]] -> [[Int]] -> SearchType -> NextNodesFunc -> Int -> IO ()
    runSearch xss os cs st nn n
        | isSolved xs = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | suc /= PQ.empty                                       = displayGrid xs >> runSearch ((max suc):xss)   (PQ.union os $ PQ.deleteMax suc)                (xs:cs) st nn (n+1)
        | suc == PQ.empty && os' /= PQ.empty                    = displayGrid xs >> runSearch ((max os'):xss)   (PQ.filter (/= (snd . PQ.findMax $ os')) os)    (xs:cs) st nn (n+1)
        | suc == PQ.empty && os' == PQ.empty && os /= PQ.empty  = displayGrid xs >> runSearch (tail xss)        os                                              (xs:cs) st nn (n+1)
        | otherwise = putErr E.NotSolvable where
            xs      = head xss
            suc     = nn st xss cs
            os'     = PQ.filter (\x -> tail x == tail xss) os
            max x   = head . snd $ PQ.findMax x

    solve :: [Int] -> (Maybe SearchType, Maybe Distance) -> IO ()
    solve xs (Nothing, Nothing) = putStrLn ("Solving grid using the Astar algorihtm and the Manhattan distance")                        >> runSearch  [xs]  PQ.empty  []  Astar  (getNextNodes Manhattan)  0
    solve xs (Just st, Nothing) = putStrLn ("Solving grid using the " ++ (show st) ++ " algorihtm and the Manhattan distance")          >> runSearch  [xs]  PQ.empty  []  st     (getNextNodes Manhattan)  0
    solve xs (Nothing, Just d)  = putStrLn ("Solving grid using the Astar algorihtm and the " ++ (show d) ++ " distance")               >> runSearch  [xs]  PQ.empty  []  Astar  (getNextNodes d)          0
    solve xs (Just st, Just d)  = putStrLn ("Solving grid using the " ++ (show st) ++ " algorihtm and the " ++ (show d) ++ " distance") >> runSearch  [xs]  PQ.empty  []  st     (getNextNodes d)          0