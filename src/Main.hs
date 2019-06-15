module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Grid (getSolvedGrid)
import Solver (solve)
import Logger
import Parser

checkArgs :: [String] -> IO ()
checkArgs xs = if null xs then displayHelp >> exitSuccess else pure ()

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    content <- readFile $ args !! 0
    case transformInput (words <$> (drop 1 . clearInput . lines $ content)) of
        Just grid   -> solve (getSolvedGrid $ length grid) grid (parseArgs args)
        Nothing     -> putErr InvalidInput