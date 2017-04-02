module Main where

import DataSet
import Lib
import PrettyPrint
import Safe
import System.Environment
import System.Exit
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    args <- getArgs
    case headMay args of
        Nothing -> printUsage >> exitFailure
        Just file -> do
            (resources, project) <- readData file
            putStr "\n\n"
            showProjectIO resources $ plan project

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " <fichero.rcp>"
