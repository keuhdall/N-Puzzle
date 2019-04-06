module Main where

import System.Environment
import qualified Error
import Logger
import Parser
import Checker

main :: IO ()
main = do
    args <- getArgs 
    content <- readFile (args !! 0)
    let l = lines content
    let w = map (\l' -> words l') $ drop 1 $ clearInput l
    case transformInput w of
        Just w' -> do
            if isSolvable (concat w') then mapM_ (\z-> putStrLn $ show z) $ concat w' else putErr Error.NotSolvable
        Nothing -> putErr Error.InvalidInput