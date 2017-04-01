module DataSet where

import Lib
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Map.Strict as M

readData :: FilePath -> IO (Int, Project)
readData fp = do
    inputData <- readFile fp
    case (parse projectParser fp inputData) of
        Left err -> putStr (parseErrorPretty err) >> exitFailure
        Right xs -> return xs

projectParser :: Parser (Int, Project)
projectParser = do
    _ <- (some digitChar) >> spaceChar >> (some digitChar) >> eol
    space
    resources <- some digitChar
    eol
    tasks <- many taskParser
    return (read resources, M.fromList $ zip (fmap Index [1..]) tasks)

taskParser :: Parser Task
taskParser = do
    span <- some digitChar
    space
    resources <- some digitChar
    space
    nsucc <- some digitChar
    space
    successors <- successorsParser $ read nsucc
    return $ Task
        False
        (read span)
        (read resources)
        (fmap Index successors)

successorsParser :: Int -> Parser [Int]
successorsParser n = do
    successors <- count n successor
    return successors

successor :: Parser Int
successor = do
    num <- some digitChar
    space
    return $ read num
