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

project :: Project
project = M.fromList testProject

testTask = Task
    False
    0
    0
    [Index 2, Index 3, Index 4]

testProject =
    [ (Index 1
      , Task
            False
            0
            0
            [Index 2, Index 3, Index 4])

    , (Index 2
      , Task
            False
            5
            3
            [Index 5])

    , (Index 3
      , Task
            False
            3
            2
            [Index 5, Index 6])

    , (Index 4
      , Task
            False
            2
            2
            [Index 6, Index 7])

    , (Index 5
      , Task
            False
            4
            2
            [Index 8])

    , (Index 6
      , Task
            False
            5
            1
            [Index 8, Index 9])

    , (Index 7
      , Task
            False
            4
            3
            [Index 9, Index 10])

    , (Index 8
      , Task
            False
            4
            4
            [Index 11])

    , (Index 9
      , Task
            False
            4
            3
            [Index 11])

    , (Index 10
      , Task
            False
            5
            2
            [Index 11])

    , (Index 11
      , Task
            False
            0
            0
            [])
    ]
