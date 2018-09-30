module Main where

import System.Environment

import Parser.Parser

main :: IO ()
main = do
    args <- getArgs 
    content <- readFile (args !! 0)
    let l = lines content
    mapM_ putStrLn l