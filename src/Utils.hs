module Utils
    (
        ascendingPriority,
        flattenMap,
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

expandId :: (Unique a) => M.Map Id a -> M.Map Id v -> M.Map a v
expandId idLookup m = M.fromList . map get $ M.toList m
    where
        get (ideed, v1) = (fromMaybe (error "Id not in lookup") (M.lookup ideed idLookup), v1)
