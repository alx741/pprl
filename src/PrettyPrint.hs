module PrettyPrint
    ( showProject
    , showProjectDebug
    , showProjectIO
    ) where

import Control.Monad
import Lib
import Rainbow
import qualified Data.Map.Strict as M

showTaskDebug :: Index -> Task -> String
showTaskDebug i t =
    "Tarea " ++
    (show $ getIndex i) ++
    "\n|_________Allocated: " ++ (show $ taskAllocated t) ++ "\n" ++
    "\n|_________Finaliza el día: " ++ (show $ taskSpan t) ++ "\n" ++
    "\n|_________Recursos necesarios: " ++ (show $ taskResources t) ++ "\n" ++
    "\n|_________Successors: " ++ (show $ taskSuccessors t) ++ "\n\n"

showProjectDebug :: Project -> String
showProjectDebug p = tasks p
  where
    tasks = concat . (M.mapWithKey showTaskDebug)

showTask :: Int ->  Index -> Task -> String
showTask maxRes i t =
    "Tarea " ++
    (show $ getIndex i) ++
    "\n|_________Finaliza el día: " ++ (show $ taskSpan t) ++ "\n" ++
    if taskResources t > maxRes
        then "\n|_________Recursos necesarios: " ++
            (show $ taskResources t) ++ " -- [!] Conflicto de recursos\n\n"
        else "\n|_________Recursos necesarios: " ++
            (show $ taskResources t) ++ "\n\n"


showProject :: Int -> Project -> String
showProject maxRes p = tasks p ++ totalSpan p
  where
    tasks = concat . (M.mapWithKey (showTask maxRes))
    totalSpan p =
        "\n\n=====================================\n" ++
        "Duración total del proyecto: " ++ (show $ maxSpan p) ++ " días" ++
        "\n=====================================\n\n"


showProjectIO :: Int -> Project -> IO ()
showProjectIO maxRes p = do
    showTasksIO (Index 1) (Index (length p)) maxRes p
    putChunkLn $ chunk (totalSpan p) & fore green
    where
        showTasksIO :: Index -> Index -> Int -> Project -> IO ()
        showTasksIO i imax maxRes p
            | i == imax = return ()
            | otherwise = do
                showTaskIO maxRes i (p M.! i)
                showTasksIO (Index $ getIndex i + 1) imax maxRes p

        totalSpan p =
            "\n=====================================\n" ++
            "Duración total del proyecto: " ++ (show $ maxSpan p) ++ " días" ++
            "\n=====================================\n\n"

showTaskIO :: Int ->  Index -> Task -> IO ()
showTaskIO maxRes i t = do
    putChunkLn $ chunk ("Tarea " ++ (show $ getIndex i)) & fore blue
    putStrLn $ "|_________Finaliza el día: " ++ (show $ taskSpan t)
    if taskResources t > maxRes
        then do
            putStr $ "|_________Recursos necesarios: " ++ (show $ taskResources t)
            putChunkLn $ chunk "  [!] Conflicto de recursos" & fore red
        else putStrLn $ "|_________Recursos necesarios: " ++
            (show $ taskResources t) ++ "\n\n"
