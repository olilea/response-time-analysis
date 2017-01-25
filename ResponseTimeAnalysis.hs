
module ResponseTimeAnalysis
    (
        responseTimeAnalysis
    )
where

import Data.List (find, sortBy)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)

import Structures

data Task = Task {
    tSpec :: TaskSpec,
    tPriority :: TaskPriority,
    tComputation :: TaskComputation
} deriving (Show)

instance Eq Task where
    (Task sp1 _ _) == (Task sp2 _ _) = tsId sp1 == tsId sp2

instance Ord Task where
    (Task sp1 _ _) `compare` (Task sp2 _ _) = tsId sp1 `compare` tsId sp2

ascendingPriority :: [Task] -> [Task]
ascendingPriority = sortBy (comparing tPriority)

scale :: ScaleFactor -> Task -> Task
scale sf t = t { tComputation = nComp }
    where nComp = tComputation t * sf

responseTimeSingleR :: Task -> M.Map Task TaskResponseTime -> Float -> TaskResponseTime
responseTimeSingleR t rts pr
    | pr > (tsDeadline . tSpec) t = Nothing
    | pr == r = Just r
    | otherwise = responseTimeSingleR t rts r
    where
        hpts = M.keys rts
        singleInterference hpt = (tComputation hpt) * ((fromIntegral . ceiling) (pr / (tsPeriod . tSpec) hpt))
        interference = sum . map singleInterference $ hpts
        r = tComputation t + interference

responseTimeSingle :: Task -> M.Map Task TaskResponseTime -> TaskResponseTime
responseTimeSingle t rts
    | rts == M.empty = Just c
    | failed = Nothing
    | otherwise = responseTimeSingleR t rts c
    where
        -- Task of higher priority misses its deadline -> current task misses deadline
        failed = isJust . find isNothing . M.elems $ rts
        c = tComputation t

responseTimesR :: [Task] -> M.Map Task TaskResponseTime -> M.Map Task TaskResponseTime
responseTimesR [] rts = rts
responseTimesR ts rts = responseTimesR remaining nrts
    where
        (highest:remaining) = ts
        nrts = M.insert highest (responseTimeSingle highest rts) rts

responseTimes :: [Task] -> M.Map Task TaskResponseTime
responseTimes []  = M.empty
responseTimes ts = responseTimesR (remaining) rts
    where
        (highest:remaining) = ascendingPriority ts
        rts = M.singleton highest $ responseTimeSingle highest M.empty

responseTimeAnalysis :: [(TaskSpec, TaskPriority, TaskComputation)] -> ScaleFactor -> M.Map TaskSpec TaskResponseTime
responseTimeAnalysis taskSet sf = M.fromList $ map toSpec $ M.toList $ responseTimes ts
    where
        ts = map ((scale sf) . toTask) taskSet
        toTask (spec, p, c) = Task spec p c
        toSpec (Task (TaskSpec idee period deadline comm) _ _, rt) = (TaskSpec idee period deadline comm, rt)

main :: IO ()
main = do
    let tasks = [((TaskSpec 1 10 10 (Communication 2 1)), 1, 2),
                ((TaskSpec 2 6 6 (Communication 3 1)), 2, 4),
                ((TaskSpec 3 7 7 (Communication 1 1)), 3, 6)]
    print $ responseTimeAnalysis tasks 1.0
