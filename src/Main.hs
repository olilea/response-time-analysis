module Main where

import CommunicationAnalysis
import ResponseTimeAnalysis
import Structures
import Utils

import qualified Data.Map as M

main :: IO ()
main = do
    -- let c = Core 1 1.0
    let cs = [(Core 1 1.0), Core 2 1.0]
    let ts = [(Task 1 10 10 1 2 (Communication 2 1)),
                (Task 2 6 6 2 4 (Communication 3 1)),
                (Task 3 7 7 3 6 (Communication 1 1))]
    let tm = M.fromList [(1, 1), (2, 1), (3, 2)]
    let cm = M.fromList [(1, (1, 1)), (2, (1, 2))]
    -- putStr $ (unlines . map show . M.toList) $ responseTimeAnalysis tasks c 1.0
    -- print $ routeXY (1, 1) (3, 3)

    let p@(_, _, _, sf) = (1, 1, 1.0, 1.0)
    let a = (cs, ts, tm, cm)

    let taskLookup = M.fromList . map (\t -> (tId t, t)) $ ts
    let responseTimes = flattenMap
                      . map (\c -> responseTimeAnalysis (tasksOnCore c a taskLookup) c sf)
                      $ cs
    let commTimes = communicationAnalysis p a responseTimes
    print $ M.elems responseTimes
    print $ M.elems commTimes
