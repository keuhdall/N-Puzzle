module Main where

import System.Environment
import System.Exit
import qualified Error as E
import Solver.Grid
import Solver.Distance
import Solver.Solver
import Logger
import Parser

checkArgs :: [String] -> IO ()
checkArgs xs = if length xs == 0 then displayHelp >> exitSuccess else return ()

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    content <- readFile $ args !! 0
    let l = lines content; w = map words $ drop 1 $ clearInput l; pargs = parseArgs args
    case transformInput w of
        Just grid   -> solve (getSolvedGrid $ length grid) grid pargs
        Nothing     -> putErr E.InvalidInput