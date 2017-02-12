
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

endToEnd :: Platform -> Application -> ScaleFactor -> M.Map Task ResponseTime
endToEnd p a@(Application cs ts _ _ pm) sf = es
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

missingDeadlines :: Platform -> Application -> ScaleFactor -> Int
missingDeadlines p sf a = length . filter missed . M.toList $ endToEnd p sf a
  where
    missed (t, mrt) = case mrt of
      Nothing -> True
      Just rt -> if rt > (tDeadline t) then True else False

schedulable :: Platform -> Application -> ScaleFactor -> Bool
schedulable p a sf = (==) 0 $ missingDeadlines p a sf 

breakdownFrequency' :: Platform
                    -> Application
                    -> Int
                    -> ScaleFactor
                    -> Maybe ScaleFactor
                    -> Maybe ScaleFactor
breakdownFrequency' p a stepsRem curSf best
  | stepsRem == 0 = best
  | otherwise = breakdownFrequency' p a (stepsRem - 1) nextSf nextBest
      where
        sched = schedulable p a curSf
        nextSf = curSf `dir` 0.10
          where dir = if sched then (-) else (+)
        nextBest = if isNothing best && sched then Just curSf else do
          (\b -> if b <= curSf then b else curSf) <$> best

breakdownFrequency :: Platform -> Application -> Int -> Maybe ScaleFactor
breakdownFrequency p a depth = breakdownFrequency' p a depth 1.0 Nothing

frequencies = [0.01, 0.02..1.99]
  ++ [2.0, 2.1..2.4]
  ++ [2.5, 3.0..4.5]
  ++ [5.0, 6.0..19.0]
  ++ [20.0, 25.0..45.0]
  ++ [50.0, 60.0..100.0]

middle :: [a] -> Maybe Int
middle [_] = Nothing
middle as = return $ div (length as) 2

bdf2' :: (ScaleFactor -> Bool) -> [ScaleFactor] -> Maybe ScaleFactor -> Int -> Int -> Maybe ScaleFactor
bdf2' sched sfs curBestSf low high
  | high < low = curBestSf
  | better = bdf2' sched sfs (return mid) low (midI - 1)
  | otherwise = bdf2' sched sfs curBestSf (midI + 1) high
  where
    midI = low + ((high - low) `div` 2)
    mid = sfs !! midI
    curSched = sched mid
    better = if curSched
      then case curBestSf of
        Nothing -> True
        Just sf -> mid < sf
      else False

bdf2 :: Platform -> Application -> Maybe ScaleFactor
bdf2 p a = bdf2' (schedulable p a) frequencies Nothing 0 (length frequencies - 1)
