module Lib
    (
    -- * Tipos de datos
    Project
    , Task(..)
    , Index(..)
    , Span
    -- * Funciones de planificación
    , plan
    , maxSpan
    ) where

import qualified Data.Map.Strict as M

type Project = M.Map Index Task


data Task = Task
    { taskAllocated :: Bool
    , taskSpan :: Span
    , taskResources :: Int
    , taskSuccessors :: [Index]
    } deriving (Show, Read)

instance Eq Task where
    (==) t1 t2 = taskSpan t1 == taskSpan t2

instance Ord Task where
    compare t1 t2
        | taskSpan t1 == taskSpan t2 = EQ
        | taskSpan t1 <= taskSpan t2 = LT
        | taskSpan t1 >= taskSpan t2 = GT

type Span = Int

newtype Index = Index { getIndex :: Int }
    deriving (Show, Read, Ord, Eq)

maxSpan :: Project -> Span
maxSpan p = taskSpan $ maximum $ fmap snd $ M.toList $ planTime p

-- |Planificar el proyecto
-- Incrementa los valores de duración y recursos para cada tarea
plan :: Project -> Project
plan p = planTime $ planRes p

-- |Realiza planificación de tiempo
planTime :: Project -> Project
planTime project = applyPlan (M.size project) 1 project
    where
        applyPlan :: Int -> Int -> Project -> Project
        applyPlan max n p
            | n == max = p
            | otherwise =
                let task = p M.! Index n
                in applyPlan max (n+1) $ applyTask task p

-- |Planificar una única tarea
-- Aplica la planificación temporal sobre todas las tareas referenciadas
applyTask :: Task -> Project -> Project
applyTask task@(Task _ span _ indexes) p = foldr calcSuccessor p indexes
    where
        calcSuccessor :: Index -> Project -> Project
        calcSuccessor index p =
            let task' = allocateTask (p M.! index) span
            in M.insert index task' p

        -- |Ubicar la tarea en el tiempo
        allocateTask :: Task -> Span -> Task
        allocateTask task@(Task True _ _ _) _ = task
        allocateTask task span = Task
            True
            (taskSpan task + span)
            (taskResources task)
            (taskSuccessors task)

-- |Realiza planificación de recursos
planRes :: Project -> Project
planRes project = applyPlan (M.size project) 1 project
    where
        applyPlan :: Int -> Int -> Project -> Project
        applyPlan max n p
            | n == max = p
            | otherwise =
                let task = p M.! Index n
                in applyPlan max (n+1) $ calcRes task p

        calcRes :: Task -> Project -> Project
        calcRes task@(Task _ _ _ (i:indexes)) p =
            allocateRes (i:indexes) 0 p

        -- |Ubicar los recursos necesarios para la tarea
        allocateRes :: [Index] -> Int -> Project -> Project
        allocateRes [] _ p = p
        allocateRes (i:is) res p =
            let task = p M.! i
                res' = taskResources task + res
                task' = Task
                    False
                    (taskSpan task)
                    (res')
                    (taskSuccessors task)
            in M.insert i task' $ allocateRes is res' p
