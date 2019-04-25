module Main where

import System.Environment
import System.Exit
import qualified Error as E
import Solver.Distance
import Solver.Methods
import Solver.Solver
import Logger
import Parser
import Checker

checkArgs :: [String] -> IO ()
checkArgs xs = case length xs of
    0 -> do
        displayHelp
        exitSuccess
    1 -> return () -- Solve puzzle with default algorithm and default heuristic
    2 -> return () -- Solve puzzle with default algorithm and custom heuristic
    _ -> return () -- Solve puzzle with custom algorithm and custom heuristic

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    content <- readFile (args !! 0)
    let l = lines content
    let w = map words $ drop 1 $ clearInput l
    case transformInput w of
        Just w' -> do
            if isSolvable $ concat w' then solve (concat w') Astar Hamming else putErr E.NotSolvable
            --if isSolvable $ concat w' then displayGrid $ concat w' else putErr E.NotSolvable
        Nothing -> putErr E.InvalidInput