module Main where

import System.Environment
import Parser

main :: IO ()
main = do
    args <- getArgs 
    content <- readFile (args !! 0)
    let l = lines content
    let w = map (\l' -> words l') $ clearInput $ drop 1 l
    case transformInput w of
        Just w' -> do
            mapM_ (\z-> putStrLn $ show z) $ concat w'
        Nothing -> putStrLn "KO"