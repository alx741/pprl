module Lib
    ( Project
    , Task(..)
    , Successor(..)
    , getSuccessor
    ) where

import Data.Vector as V

type Project = V.Vector Task

data Task = Task
    { allocated :: Bool
    , span :: Int
    , resources :: Int
    , successors :: [Successor]
    } deriving (Show, Read)

newtype Successor =
    Successor Int
    deriving (Show, Read)

getSuccessor :: Successor -> Int
getSuccessor (Successor n) = n-1
