module PrettyPrint
    ( showTask
    , showProject
    ) where

import qualified Data.Map.Strict as M
import Lib

showTask :: Index -> Task -> String
showTask i t =
    "Tarea " ++
    (show $ getIndex i) ++
    "\n|_________Finaliza el día: " ++ (show $ taskSpan t) ++ "\n\n"

showProject :: Project -> String
showProject p = tasks p ++ totalSpan p
  where
    tasks = concat . (M.mapWithKey showTask)
    totalSpan p =
        "\n\n=====================================\n" ++
        "Duración total del proyecto: " ++ (show $ maxSpan p) ++ " días" ++
        "\n=====================================\n\n"
