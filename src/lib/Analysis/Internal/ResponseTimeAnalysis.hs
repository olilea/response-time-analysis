
module Analysis.Internal.ResponseTimeAnalysis
where

import Data.List (find)
import qualified Data.Map as M
import Data.Maybe

import Analysis.Internal.Structures
import Analysis.Internal.Utils

type ResponseMap = M.Map Task ResponseTime

scale :: ScaleFactor -> Task -> Task
scale sf t = t { tComputation = nComp }
    where nComp = tComputation t * sf

responseTimeSingleR :: Task -> ResponseMap -> Float -> ResponseTime
responseTimeSingleR t rts pr
    | pr > (tDeadline t) * 2 = Nothing
    | pr == r = Just r
    | otherwise = responseTimeSingleR t rts r
    where
        singleInterference hpt = tComputation hpt *
            fromIntegral (ceiling (pr / tPeriod hpt) :: Int)
        interference = sum . map singleInterference . M.keys $ rts
        r = tComputation t + interference

responseTimeSingle :: Task -> ResponseMap -> ResponseTime
responseTimeSingle t rts
    | rts == M.empty = highestPriorityResponse
    | failed = Nothing
    | otherwise = responseTimeSingleR t rts c
    where
        -- Higher priority task could not have a response-time calculated
        -- means that this task will also not have a response-time calculated
        failed = isJust . find isNothing . M.elems $ rts
        c = tComputation t
        -- Must be the highest priority task -> R = C
        highestPriorityResponse = Just c

responseTimesR :: [Task] -> ResponseMap -> ResponseMap
responseTimesR [] rts = rts
responseTimesR ts rts = responseTimesR remaining nrts
    where
        (highest:remaining) = ts
        nrts = M.insert highest (responseTimeSingle highest rts) rts

responseTimes :: TaskPriorityMapping -> [Task] -> M.Map Task ResponseTime
responseTimes _ []  = M.empty
responseTimes pm ts = responseTimesR remaining rts
    where
        (highest:remaining) = descendingPriority ts pm
        rts = M.singleton highest $ responseTimeSingle highest M.empty

responseTimeAnalysis :: [Task] -> TaskPriorityMapping -> Core -> ScaleFactor -> ResponseMap
responseTimeAnalysis ts pm c sf = responseTimes pm . map (scale (1.0 / (cSpeed c * sf))) $ ts
