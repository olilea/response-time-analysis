
module Analysis 
    ( module Analysis
    , module Analysis.Internal.Structures
    )
    where

import Analysis.Internal.CommunicationAnalysis
import Analysis.Internal.ResponseTimeAnalysis
import Analysis.Internal.Structures
import Analysis.Internal.Utils

import qualified Data.Map as M
import Data.Maybe

import Debug.Trace

endToEnd :: Platform -> ScaleFactor -> Application -> M.Map Task ResponseTime
endToEnd p sf a@(Application cs ts _ _ pm) = es
    where
        taskLookup = M.fromList. map (\t -> (tId t, t)) $ ts
        rts = flattenMap
                    . map (\c -> responseTimeAnalysis (tasksOnCore c a taskLookup) pm c sf)
                    $ cs
        commTimes = communicationAnalysis p sf a rts
        es = M.fromList
            . map (\(t, rt, ct) -> (t, combine t rt ct))
            . map (\t -> (t, (fromJust . M.lookup t) rts, (fromJust . M.lookup t) commTimes))
            $ ts
        combine :: Task -> ResponseTime -> ResponseTime -> ResponseTime
        combine t r c
            | isNothing r || isNothing c = Nothing
            | otherwise = if total <= tDeadline t then Just total else Nothing
                where total = fromJust r + fromJust c

missingDeadlines :: Platform -> ScaleFactor -> Application -> Int
missingDeadlines p sf a = length . filter missed . M.toList $ endToEnd p sf a
  where
    missed (t, mrt) = case mrt of
      Nothing -> True
      Just rt -> if rt > (tDeadline t) then True else False

schedulable :: Platform -> ScaleFactor -> Application -> Bool
schedulable p sf a = (==) 0 $ missingDeadlines p sf a

breakdownFrequency' :: Platform -> Application -> Int -> ScaleFactor -> Maybe ScaleFactor
breakdownFrequency' p a stepsRem curSf
  | stepsRem == 0 = if sched then Just curSf else Nothing
  | otherwise = breakdownFrequency' p a (stepsRem - 1) nextSf
      where
        sched = schedulable p curSf a
        nextSf = curSf `dir` 0.05
          where dir = if sched then (-) else (+)

breakdownFrequency :: Platform -> Application -> Int -> Maybe ScaleFactor
breakdownFrequency p a depth = breakdownFrequency' p a depth 1.0
