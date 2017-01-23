import Debug.Trace

import qualified Data.Map as Map

import Data.List (sortBy)
import Data.Ord (comparing)

data Communication = Communication {
    destination :: Task,
    size :: Int
} deriving (Show)

data TaskSpec = TaskSpec {
    tsPeriod :: Int,
    tsDeadline :: Int,
    tsReleaseJitter :: Int,
    tsComm :: Communication
} deriving (Show)

data Task = Task {
    tPriority :: Int,
    tComputation :: Int,
    tPeriod :: Int,
    tDeadline :: Int,
    tReleaseJitter :: Int
} deriving (Show)

instance Eq Task where
    (Task p1 _ _ _ _) == (Task p2 _ _ _ _) = p1 == p2

instance Ord Task where
    (Task p1 _ _ _ _) `compare` (Task p2 _ _ _ _) = p1 `compare` p2

ascendingPriority :: [Task] -> [Task]
ascendingPriority = sortBy (comparing tPriority)

responseTimeSingleR :: Map.Map Task Int -> Task -> Int -> Int
responseTimeSingleR rts t pr
    | pr > tDeadline t = -1
    | pr == r = r
    | otherwise = responseTimeSingleR rts t r
    where
        hpts = Map.keys rts
        singleInterference = \hpt -> (tComputation hpt) * (ceiling ((fromIntegral pr) / (fromIntegral . tPeriod) hpt))
        interference = sum $ map singleInterference hpts
        r = tComputation t + interference

responseTimeSingle :: Map.Map Task Int -> Task -> Int
responseTimeSingle rts t
    | rts == Map.empty = c
    | otherwise = responseTimeSingleR rts t c
    where
        c = tComputation t

responseTimesR :: [Task] -> Map.Map Task Int -> Map.Map Task Int
responseTimesR [] rts = rts
responseTimesR ts rts = responseTimesR remaining nrts
    where
        remaining = tail ts
        highest = head ts
        nrts = Map.insert highest (responseTimeSingle rts highest) rts

responseTimes :: [Task] -> Map.Map Task Int
responseTimes [] = Map.empty
responseTimes [t] = Map.singleton t (tComputation t)
responseTimes ts = responseTimesR (tail tss) rts
    where
        tss = ascendingPriority ts
        highest = head tss
        rts = Map.singleton highest $ responseTimeSingle Map.empty $ highest

main = do
    print $ responseTimes [Task 1 1 10 10 5, Task 2 2 6 6 6, Task 3 2 7 7 7]