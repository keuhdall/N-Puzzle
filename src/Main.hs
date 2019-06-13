module Main where

import System.Environment
import System.Exit
import System.Random
import Data.Array.IO
import Control.Monad
import Grid
import Solver
import Logger
import Parser

shuffle :: [Int] -> IO [Int]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

checkArgs :: [String] -> IO ()
checkArgs xs = if length xs == 0 then shuffle [0..8] >>= (\x -> pure $ chunkList 3 x) >>= (\grid -> solve (getSolvedGrid $ length grid) grid (Nothing, Nothing)) >> exitSuccess else pure ()

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    content <- readFile $ args !! 0
    case transformInput (words <$> (drop 1 . clearInput . lines $ content)) of
        Just grid   -> solve (getSolvedGrid $ length grid) grid (parseArgs args)
        Nothing     -> putErr InvalidInput