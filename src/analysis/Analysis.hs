
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
