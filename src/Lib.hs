module Lib
    ( Project
    , Task(..)
    , Index(..)
    , Span
    , index
    ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

type Project = M.Map Index Task

type Span = Int

data Task = Task
    { taskAllocated :: Bool
    , taskSpan :: Span
    , taskResources :: Int
    , taskSuccessors :: [Index]
    } deriving (Show, Read)

newtype Index =
    Index Int
    deriving (Show, Read, Ord, Eq)

index :: Index -> Int
index (Index n) = n-1
