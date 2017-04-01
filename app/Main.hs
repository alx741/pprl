module Main where

import Lib
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Debug.Trace

main :: IO ()
main = print $ plan (M.size project) 1 project


plan :: Int -> Int -> Project -> Project
plan max n project
    | n == max = project
    | otherwise = plan max (n+1) $ applyTask task project
    where
        task = project M.! (Index n)

applyTask :: Task -> Project -> Project
applyTask (Task _ span _ indexes) p = foldr calcSuccessor p indexes
    where
        calcSuccessor :: Index -> Project -> Project
        calcSuccessor index p =
            let task' = allocateTask (p M.! index) span
            in M.insert index task' p

        allocateTask :: Task -> Span -> Task
        allocateTask task@(Task True _ _ _) _ = task
        allocateTask task span = Task
            True
            (taskSpan task + span)
            (taskResources task)
            (taskSuccessors task)




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
