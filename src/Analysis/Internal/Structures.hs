module Analysis.Internal.Structures
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

type ResponseTime = Maybe Float

type BasicLatency = Float

type ScaleFactor = Float

type FlitSize = Float
type LinkDelay = Float
type RoutingDelay = Float

data Location = Location Int Int
    deriving (Eq, Show)
-- Source and destination
data Link = Link Location Location
    deriving (Eq, Show)
-- Zero indexed from the top-left
type TrafficFlow = [Link]

type CoreMapping = M.Map CoreId Location
type TaskMapping = M.Map TaskId CoreId
type PriorityMapping = M.Map TaskId TaskPriority

data Platform = Platform FlitSize LinkDelay RoutingDelay
    deriving (Eq, Show)
data Application = Application [Core] [Task] TaskMapping CoreMapping PriorityMapping
    deriving (Eq, Show)

class (Ord a) => Unique a where
    idee :: a -> Id

data Communication = Communication {
    cDestination :: TaskId,
    cSize :: Int
} deriving (Show)

data Task = Task {
    tId :: Id,
    tPeriod :: TaskPeriod,
    tDeadline :: TaskDeadline,
    tComputation :: TaskComputation,
    tCommunication :: Communication
} deriving (Show)

instance Unique Task where
    idee = tId

instance Eq Task where
    (Task id1 _ _ _ _) == (Task id2 _ _ _ _) = id1 == id2

instance Ord Task where
    (Task id1 _ _ _ _) `compare` (Task id2 _ _ _ _) = id1 `compare` id2

data Core = Core {
    cId :: Id,
    cSpeed :: Float
} deriving (Show)

instance Unique Core where
    idee = cId

instance Eq Core where
    (Core id1 _) == (Core id2 _) = id1 == id2
instance Ord Core where
    (Core id1 _) `compare` (Core id2 _) = id1 `compare` id2

