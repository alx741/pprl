module Main where

import Lib
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main = print "main"

total :: Project -> Project
-- total p = M.foldr calc p p
total pro = fun 11 $ fun 10 $ fun 9 $ fun 8 $ fun 7 $ fun 6 $ fun 5 $ fun 4 $ fun 3 $ fun 2 $ fun 1 pro
    where fun = (\i p -> calc (p M.! (Index i)) p)

calc :: Task -> Project -> Project
calc (Task _ span _ indexes) p = foldr calcOne p indexes
    where
        calcOne index p =
            let task' = allocateTask (p M.! index) span
            in M.insert index task' p

getTasks :: Project -> [Index] -> [Task]
getTasks p = fmap (p M.!)

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
