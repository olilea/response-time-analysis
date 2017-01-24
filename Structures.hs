module Structures
    (
        Communication(cDestination, cSize),
        TaskSpec(tsId, tsPeriod, tsDeadline),
        TaskId,
        TaskPeriod,
        TaskDeadline,
        TaskPriority,
        TaskComputation,
        TaskResponseTime,
        ScaleFactor
    )
where

data Communication = Communication {
    cDestination :: TaskSpec,
    cSize :: Int
} deriving (Show)

data TaskSpec = TaskSpec {
    tsId :: TaskId,
    tsPeriod :: TaskPeriod,
    tsDeadline :: TaskDeadline
} deriving (Show)

instance Eq TaskSpec where
    (TaskSpec id1 _ _) == (TaskSpec id2 _ _) = id1 == id2

instance Ord TaskSpec where
    (TaskSpec id1 _ _) `compare` (TaskSpec id2 _ _) = id1 `compare` id2

type CommunicationSize = Int

type TaskId = Int
type TaskPeriod = Float
type TaskDeadline = Float

type TaskPriority = Int
type TaskComputation = Float

type TaskResponseTime = Maybe Float
type ScaleFactor = Float