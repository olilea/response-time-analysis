
module ResponseTimeAnalysis
    (
        responseTimeAnalysis
    )
where

import Data.List (find)
import qualified Data.Map as M
import Data.Maybe

import Structures
import Utils

scale :: ScaleFactor -> Task -> Task
scale sf t = t { tComputation = nComp }
    where nComp = tComputation t * sf

responseTimeSingleR :: Task -> TaskResponseTimes -> Float -> TaskResponseTime
responseTimeSingleR t rts pr
    | pr > tDeadline t = Nothing
    | pr == r = Just r
    | otherwise = responseTimeSingleR t rts r
    where
        hpts = M.keys rts
        singleInterference hpt = (tComputation hpt) * ((fromIntegral . ceiling) (pr / (tPeriod hpt)))
        interference = sum . map singleInterference $ hpts
        r = tComputation t + interference

responseTimeSingle :: Task -> TaskResponseTimes -> TaskResponseTime
responseTimeSingle t rts
    | rts == M.empty = singleResponse
    | failed = Nothing
    | otherwise = responseTimeSingleR t rts c
    where
        -- Task of higher priority misses its deadline -> current task misses deadline
        failed = isJust . find isNothing . M.elems $ rts
        c = tComputation t
        singleResponse = if tComputation t > tDeadline t then Nothing else (Just . tComputation) $ t

responseTimesR :: [Task] -> TaskResponseTimes -> TaskResponseTimes
responseTimesR [] rts = rts
responseTimesR ts rts = responseTimesR remaining nrts
    where
        (highest:remaining) = ts
        nrts = M.insert highest (responseTimeSingle highest rts) rts

responseTimes :: [Task] -> TaskResponseTimes
responseTimes []  = M.empty
responseTimes ts = responseTimesR remaining rts
    where
        (highest:remaining) = ascendingPriority ts
        rts = M.singleton highest $ responseTimeSingle highest M.empty

responseTimeAnalysis :: [Task] -> Core -> ScaleFactor -> TaskResponseTimes
responseTimeAnalysis ts c sf = responseTimes . map (scale sf . scale (1.0 / cSpeed c)) $ ts
