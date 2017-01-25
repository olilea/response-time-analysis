module Structures
    (
        Id,
        TaskId,
        TaskPeriod,
        TaskDeadline,
        TaskPriority,
        TaskComputation,
        TaskResponseTime,
        ScaleFactor,
        Communication(..),
        TaskSpec(..),
    )
where

type Id = Int

type TaskId = Id
type TaskPeriod = Float
type TaskDeadline = Float

type TaskPriority = Int
type TaskComputation = Float

type TaskResponseTime = Maybe Float
type ScaleFactor = Float


data Communication = Communication {
    cDestination :: TaskId,
    cSize :: Int
} deriving (Show)

data TaskSpec = TaskSpec {
    tsId :: TaskId,
    tsPeriod :: TaskPeriod,
    tsDeadline :: TaskDeadline,
    tsCommunication :: Communication
} deriving (Show)

instance Eq TaskSpec where
    (TaskSpec id1 _ _ _) == (TaskSpec id2 _ _ _) = id1 == id2

instance Ord TaskSpec where
    (TaskSpec id1 _ _ _) `compare` (TaskSpec id2 _ _ _) = id1 `compare` id2
