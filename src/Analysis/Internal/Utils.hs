module Analysis.Internal.Utils
    (
        debugOut,
        descendingPriority,
        flattenMap,
        fetch,
        expandId,
        tasksOnCore
    )
where

import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)

import Debug.Trace

import Analysis.Internal.Structures

debugOut :: (Show a) => a -> a
debugOut a = traceShow a a

descendingPriority :: [Task] -> PriorityMapping -> [Task]
descendingPriority ts pm = map fst ps
    where
        ps = sortBy (comparing snd) . map (\t -> (t, fetch pm (tId t))) $ ts

flattenMap :: Ord k => [M.Map k a] -> M.Map k a
flattenMap = M.fromList . concatMap M.toList

fetch :: (Ord a) => M.Map a b -> a -> b
fetch m k = fromJust . M.lookup k $ m

expandId :: (Unique a) => M.Map Id a -> M.Map Id v -> M.Map a v
expandId idLookup = M.mapKeys fromId
    where
        fromId x = fromJust . M.lookup x $ idLookup

tasksOnCore :: Core -> Application -> M.Map TaskId Task -> [Task]
tasksOnCore c (Application _ _ tm _ _) taskLookup = map (toTask . fst) . filter isOnCore . M.toList $ tm
    where
        isOnCore (_, coreId) = coreId == cId c
        toTask task = fromJust $ M.lookup task taskLookup
