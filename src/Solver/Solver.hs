module Solver.Solver (SearchType(..), readSearchType, solve) where
    import Data.Sort
    import qualified Data.PQueue.Prio.Max as PQ
    import qualified Data.HashSet as S
    import qualified Error as E
    import Logger
    import Solver.Grid
    import Solver.Distance

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type DistFunc = Grid -> Grid -> Int
    type NextNodesFunc = [Grid] -> S.Set Grid -> PQ.MaxPQueue Int [Grid]

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
    getCost :: SearchType -> DistFunc -> [Grid] -> Grid -> Int
    getCost st d xss grid =
        let xs    =  head xss
            svd   =  getSolvedGrid $ getPuzzleSize xs
            val   =  fromCoordinates xs coord
            dist  =  d coord (getCoordinates svd val) in case st of
            Astar     -> dist + (length xss)    -- A* : h cost + g cost
            Uniform   -> (length xss)           -- Uniform : g cost only
            Greedy    -> dist                   -- Greedy : h cost only

    -- Returns a PQueue containing the next nodes (value + cost)
    getNextNodes :: Distance -> SearchType -> [Grid] -> S.Set Grid -> PQ.MaxPQueue Int [Grid]
    getNextNodes d st xss cs = getNextNodes' (getNeighbors xs) PQ.empty where
        xs    = head xss
        cost  = getCost st (getDistance d) xss
        value x = (swapValues (fromCoordinates xs x) xs):xss
        getNextNodes' (y:[]) pq = PQ.filter ((flip $ S.notMember . head) cs) $ PQ.insert (cost y) (value y) pq
        getNextNodes' (y:ys) pq = getNextNodes' ys (PQ.insert (cost y) (value y) pq)

    -- Runs the search using a given SearchType. The SearchType will be used in nodes cost computation
    runSearch :: [Grid] -> PQ.MaxPQueue Int [Grid] -> S.Set Grid -> NextNodesFunc -> Int -> IO ()
    runSearch xss os cs nn n
        | isSolved xs = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | suc /= PQ.empty                                       = runSearch ((max suc):xss)   (PQ.union os $ PQ.deleteMax suc)                cs' nn (n+1)
        | suc == PQ.empty && os' /= PQ.empty                    = runSearch ((max os'):xss)   (PQ.filter (/= (snd . PQ.findMax $ os')) os)    cs' nn (n+1)
        | suc == PQ.empty && os' == PQ.empty && os /= PQ.empty  = runSearch (tail xss)        os                                              cs' nn (n+1)
        | otherwise = putErr E.NotSolvable where
            xs      = head xss
            suc     = nn xss cs
            os'     = PQ.filter (\x -> tail x == tail xss) os
            max x   = head . snd $ PQ.findMax x
            cs'     = S.insert xs cs

    solve :: Grid -> (Maybe SearchType, Maybe Distance) -> IO ()
    solve xs (Nothing, Nothing) = putStrLn ("Solving grid using the Astar algorihtm and the Manhattan distance")                        >> runSearch  [xs]  PQ.empty  S.empty  ( getNextNodes Manhattan Astar  )  0
    solve xs (Just st, Nothing) = putStrLn ("Solving grid using the " ++ (show st) ++ " algorihtm and the Manhattan distance")          >> runSearch  [xs]  PQ.empty  S.empty  ( getNextNodes Manhattan st     )  0
    solve xs (Nothing, Just d)  = putStrLn ("Solving grid using the Astar algorihtm and the " ++ (show d) ++ " distance")               >> runSearch  [xs]  PQ.empty  S.empty  ( getNextNodes d         Astar  )  0
    solve xs (Just st, Just d)  = putStrLn ("Solving grid using the " ++ (show st) ++ " algorihtm and the " ++ (show d) ++ " distance") >> runSearch  [xs]  PQ.empty  S.empty  ( getNextNodes d         st     )  0