module Lib
    ( Project
    , Task(..)
    , Index(..)
    , Span
    , plan
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

plan :: Project -> Project
plan project = applyPlan (M.size project) 1 project
    where
        applyPlan :: Int -> Int -> Project -> Project
        applyPlan max n p
            | n == max = p
            | otherwise =
                let task = p M.! Index n
                in applyPlan max (n+1) $ applyTask task p

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
