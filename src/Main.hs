module Main where

import System.Environment
import System.Exit
import qualified Error as E
import Solver.Distance
import Solver.Solver
import Logger
import Parser

defaultSearch :: SearchType
defaultSearch = Astar

defaultHeuristic :: Distance
defaultHeuristic = Manhattan

checkArgs :: [String] -> IO ()
checkArgs xs = if (length xs) == 0 then displayHelp >> exitSuccess else return ()

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    content <- readFile (args !! 0)
    let l = lines content
    let w = map words $ drop 1 $ clearInput l
    let pargs = parseArgs args
    case transformInput w of
        Just grid -> solve grid pargs
        Nothing -> putErr E.InvalidInput