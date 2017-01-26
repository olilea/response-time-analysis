module Utils
    (
        ascendingPriority,
        flattenMap
    )
where

import Data.List (sortBy)
import qualified Data.Map as M
import Data.Ord (comparing)

import Structures

ascendingPriority :: [Task] -> [Task]
ascendingPriority = sortBy (comparing tPriority)

flattenMap :: Ord k => [M.Map k a] -> M.Map k a
flattenMap = M.fromList . concatMap M.toList