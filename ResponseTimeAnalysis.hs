
module ResponseTimeAnalysis where

import Data.List (find, sortBy)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)

import Structures

data Task = Task {
    tId :: TaskId,
    tPriority :: TaskPriority,
    tComputation :: TaskComputation,
    tPeriod :: TaskPeriod,
    tDeadline :: TaskDeadline
} deriving (Show)

convert :: TaskSpec -> TaskPriority -> TaskComputation -> Task
convert  ts priority computation =
    Task (tsId ts) priority computation (tsPeriod ts) (tsDeadline ts)

instance Eq Task where
    (Task id1 _ _ _ _) == (Task id2 _ _ _ _) = id1 == id2

instance Ord Task where
    (Task id1 _ _ _ _) `compare` (Task id2 _ _ _ _) = id1 `compare` id2

ascendingPriority :: [Task] -> [Task]
ascendingPriority = sortBy (comparing tPriority)

scale :: ScaleFactor -> Task -> Task
scale sf t = t { tComputation = nComp }
    where nComp = tComputation t * sf

responseTimeSingleR :: Task -> M.Map Task TaskResponseTime -> Float -> TaskResponseTime
responseTimeSingleR t rts pr
    | pr > tDeadline t = Nothing
    | pr == r = Just r
    | otherwise = responseTimeSingleR t rts r
    where
        hpts = M.keys rts
        singleInterference = \hpt -> (tComputation hpt) * ((fromIntegral . ceiling) (pr / tPeriod hpt))
        interference = sum $ map singleInterference hpts
        r = tComputation t + interference

responseTimeSingle :: Task -> M.Map Task TaskResponseTime -> TaskResponseTime
responseTimeSingle t rts
    | rts == M.empty = Just c
    | failed = Nothing
    | otherwise = responseTimeSingleR t rts c
    where
        failed = isJust $ find isNothing $ M.elems rts
        c = tComputation t

responseTimesR :: [Task] -> M.Map Task TaskResponseTime -> M.Map Task TaskResponseTime
responseTimesR [] rts = rts
responseTimesR ts rts = responseTimesR remaining nrts
    where
        remaining = tail ts
        highest = head ts
        nrts = M.insert highest (responseTimeSingle highest rts) rts

responseTimes :: [Task] -> ScaleFactor -> M.Map Task TaskResponseTime
responseTimes [] _  = M.empty
responseTimes ts sf = responseTimesR (tail tss) rts
    where
        tss = ascendingPriority $ map (scale sf) ts
        highest = head tss
        rts = M.singleton highest $ responseTimeSingle highest M.empty

main = do
    print $ responseTimes [Task 1 1 2 10 10, Task 2 2 4 6 6, Task 3 3 6 7 7] 0.5