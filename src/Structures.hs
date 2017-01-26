module Structures
    (module Structures)
where

import qualified Data.Map as M

type Id = Int


type CoreId = Id
type CoreSpeed = Float

type TaskId = Id
type TaskPeriod = Float
type TaskDeadline = Float
type TaskPriority = Int
type TaskComputation = Float

type TaskResponseTime = Maybe Float
type TaskResponseTimes = M.Map Task TaskResponseTime
type EndToEndResponseTime = Maybe Float
type EndToEndResponseTimes = M.Map Task EndToEndResponseTime

type ScaleFactor = Float

type FlitSize = Int
type LinkBandwidth = Int
type ProcessingDelay = Float

type Location = (Int, Int)
-- Source and destination
type Link = (Location, Location)
-- Zero indexed from the top-left
type TrafficFlow = [Link]

type CoreMapping = M.Map CoreId Location
type TaskMapping = M.Map TaskId CoreId

type Platform = (FlitSize, LinkBandwidth, ProcessingDelay, ScaleFactor)
type Application = ([Core], [Task], TaskMapping, CoreMapping)

data Task = Task {
    tId :: TaskId,
    tPeriod :: TaskPeriod,
    tDeadline :: TaskDeadline,
    tPriority :: TaskPriority,
    tComputation :: TaskComputation,
    tCommunication :: Communication
} deriving (Show)

instance Eq Task where
    (Task id1 _ _ _ _ _) == (Task id2 _ _ _ _ _) = id1 == id2

instance Ord Task where
    (Task id1 _ _ _ _ _) `compare` (Task id2 _ _ _ _ _) = id1 `compare` id2

data Core = Core {
    cId :: CoreId,
    cSpeed :: Float
}

instance Eq Core where
    (Core id1 _) == (Core id2 _) = id1 == id2
instance Ord Core where
    (Core id1 _) `compare` (Core id2 _) = id1 `compare` id2

data Communication = Communication {
    cDestination :: TaskId,
    cSize :: Int
} deriving (Show)
