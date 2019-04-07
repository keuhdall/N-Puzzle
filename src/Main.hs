module Main where

import System.Environment
import qualified Error as E
import Logger
import Parser
import Checker

main :: IO ()
main = do
    args <- getArgs 
    content <- readFile (args !! 0)
    let l = lines content
    let w = map words $ drop 1 $ clearInput l
    case transformInput w of
        Just w' -> do
            if isSolvable $ concat w' then mapM_ (putStrLn . show) $ concat w' else putErr E.NotSolvable
        Nothing -> putErr E.InvalidInput