module Main where

import System.Environment

main :: IO ()
main = do
    args <- getArgs 
    content <- readFile (args !! 0)
    let l = lines content
    mapM_ putStrLn l