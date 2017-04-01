module Main where

import Lib
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Debug.Trace

main :: IO ()
main = print $ plan (M.size project) 1 project


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
