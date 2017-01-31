
module Analysis 
    ( module Analysis
    , module Structures
    )
    where

import Internal.CommunicationAnalysis
import Internal.ResponseTimeAnalysis
import Internal.Utils

import Structures

import qualified Data.Map as M
import Data.Maybe

endToEnd :: Platform -> Application -> M.Map Task ResponseTime
endToEnd p@(_, _, _, sf) a@(cs, ts, _, _) = es
    where
        taskLookup = M.fromList. map (\t -> (tId t, t)) $ ts
        responseTimes = flattenMap
                    . map (\c -> responseTimeAnalysis (tasksOnCore c a taskLookup) c sf)
                    $ cs
        commTimes = communicationAnalysis p a responseTimes

        combine :: Task -> ResponseTime -> ResponseTime -> ResponseTime
        combine t r c
            | isNothing r || isNothing c = Nothing
            | otherwise = if total <= tDeadline t then Just total else Nothing
                where total = fromJust r + fromJust c

        es = M.fromList
            . map (\(t, rt, ct) -> (t, combine t rt ct))
            . map (\t -> (t, (fromJust . M.lookup t) responseTimes, (fromJust . M.lookup t) commTimes))
            $ ts

