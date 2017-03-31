module Main where

import Lib
import Data.Vector as V

main :: IO ()
main = print project

project :: Project
project = V.fromList testProject

testProject =
    [ Task
        False
        0
        0
        [(Successor 2), (Successor 3), (Successor 4)]

    , Task
        False
        5
        3
        [(Successor 5)]

    , Task
        False
        3
        2
        [(Successor 5), (Successor 6)]

    , Task
        False
        2
        2
        [(Successor 6), (Successor 7)]

    , Task
        False
        4
        2
        [(Successor 8)]

    , Task
        False
        5
        1
        [(Successor 8), (Successor 9)]

    , Task
        False
        4
        3
        [(Successor 9), (Successor 10)]

    , Task
        False
        4
        4
        [(Successor 11)]

    , Task
        False
        4
        3
        [(Successor 11)]

    , Task
        False
        5
        2
        [(Successor 11)]

    , Task
        False
        0
        0
        []
    ]
