module Utils
    (
        ascendingPriority,
        flattenMap,
        fetch,
        expandId
    )
where

import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)

import Structures

ascendingPriority :: [Task] -> [Task]
ascendingPriority = sortBy (comparing tPriority)

flattenMap :: Ord k => [M.Map k a] -> M.Map k a
flattenMap = M.fromList . concatMap M.toList

fetch :: (Ord a) => M.Map a b -> a -> b
fetch m k = fromMaybe (error "Not in map") . M.lookup k $ m

expandId :: (Unique a) => M.Map Id a -> M.Map Id v -> M.Map a v
expandId idLookup = M.mapKeys fromId
    where
        fromId x = fromMaybe (error "Id not in lookup") . M.lookup x $ idLookup
