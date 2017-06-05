module Main where

import Rest
import System.Directory

main :: IO ()
main = do
   putStrLn "Starting server..."
   startServer
