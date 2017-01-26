module Utils (module Utils) where

import Data.List (sortBy)
import Data.Ord (comparing)

import Structures

ascendingPriority :: [Task] -> [Task]
ascendingPriority = sortBy (comparing tPriority)