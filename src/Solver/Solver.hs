module Solver.Solver (SearchType(..), readSearchType, solve) where
    import Data.Sort
    import qualified Data.PQueue.Prio.Min as PQ
    import qualified Data.HashSet as S
    import qualified Error as E
    import Logger
    import Solver.Grid
    import Solver.Distance

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type NextNodesFunc = [Grid] -> PQ.MinPQueue Int [Grid]

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
    getCost :: SearchType -> Distance -> [Grid] -> Grid -> Grid -> Int
    getCost st d xss grid goal = let dist = calcDistance d grid goal in case st of
        Astar     -> dist + length xss  -- A* : h cost + g cost
        Uniform   -> length xss         -- Uniform : g cost only
        Greedy    -> dist               -- Greedy : h cost only

    -- Returns a PQueue containing the next nodes (value + cost)
    getNextNodes :: Grid -> Distance -> SearchType -> [Grid] -> PQ.MinPQueue Int [Grid]
    getNextNodes goal d st xss = PQ.fromList $ zip costs neighbors where
        costs = map (getCost st d xss goal) $ map head neighbors
        neighbors = map (:xss) $ getNeighbors $ head xss

    -- Runs the search using a given SearchType. The SearchType will be used in nodes cost computation
    runSearch :: Grid -> [Grid] -> PQ.MinPQueue Int [Grid] -> S.Set Grid -> NextNodesFunc -> Int -> IO ()
    runSearch goal xss os cs nn n
        | xs == goal = displayGrid xs >> putStrLn ("Solved in " ++ (show n) ++ " steps.")
        | suc /= PQ.empty                                       = displayGrid xs >> runSearch   goal   ((min suc):xss)   (PQ.union os $ PQ.deleteMin suc)             cs' nn (n+1)
        | suc == PQ.empty && os' /= PQ.empty                    = displayGrid xs >> runSearch   goal   ((min os'):xss)   (PQ.filter (/= (snd $ PQ.findMin os')) os)   cs' nn (n+1)
        | suc == PQ.empty && os' == PQ.empty && os /= PQ.empty  = displayGrid xs >> runSearch   goal   (tail xss)        os                                           cs' nn (n+1)
        | otherwise = putErr E.NotSolvable where
            xs      = head xss
            suc     = PQ.filter (\x -> S.notMember (head x) cs) $ nn xss
            os'     = PQ.filter (\x -> tail x == tail xss && S.notMember (head x) cs) os
            min x   = head . snd $ PQ.findMin x
            cs'     = S.insert xs cs

    defaultSearch :: SearchType
    defaultSearch = Astar

    defaultHeuristic :: Distance
    defaultHeuristic = Manhattan

    solve :: Grid -> Grid -> (Maybe SearchType, Maybe Distance) -> IO ()
    solve goal xs (Nothing, Nothing) = putStrLn ( "Solving grid using the " ++ (show defaultSearch) ++ " algorihtm and the " ++ (show defaultHeuristic) ++ " distance"  )  >>  runSearch goal [xs]  PQ.empty  S.empty  ( getNextNodes goal defaultHeuristic defaultSearch )  0
    solve goal xs (Just st, Nothing) = putStrLn ( "Solving grid using the " ++ (show st) ++ " algorihtm and the " ++ (show defaultHeuristic) ++ " distance"             )  >>  runSearch goal [xs]  PQ.empty  S.empty  ( getNextNodes goal defaultHeuristic st            )  0
    solve goal xs (Nothing, Just d)  = putStrLn ( "Solving grid using the " ++ (show defaultSearch) ++ " algorihtm and the " ++ (show d) ++ " distance"                 )  >>  runSearch goal [xs]  PQ.empty  S.empty  ( getNextNodes goal d                defaultSearch )  0
    solve goal xs (Just st, Just d)  = putStrLn ( "Solving grid using the " ++ (show st) ++ " algorihtm and the " ++ (show d) ++ " distance"                            )  >>  runSearch goal [xs]  PQ.empty  S.empty  ( getNextNodes goal d                st            )  0