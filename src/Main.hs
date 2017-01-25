module Main where

import ResponseTimeAnalysis
import Structures

import qualified Data.Map as M

main :: IO ()
main = do
    let tasks = [((TaskSpec 1 10 10 (Communication 2 1)), 1, 2),
                ((TaskSpec 2 6 6 (Communication 3 1)), 2, 4),
                ((TaskSpec 3 7 7 (Communication 1 1)), 3, 6)]
    putStr $ (unlines . map show . M.toList) $ responseTimeAnalysis tasks 1.0