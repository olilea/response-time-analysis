import Debug.Trace

import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord (comparing)


data Communication = Communication {
    destination :: Task,
    size :: Int
} deriving (Show)

data TaskSpec = TaskSpec {
    tsPeriod :: Int,
    tsDeadline :: Int,
    tsComm :: Communication
} deriving (Show)

data Task = Task {
    tPriority :: Int,
    tComputation :: Float,
    tPeriod :: Float,
    tDeadline :: Float
} deriving (Show)

type ResponseTime = Maybe Float
type ScaleFactor = Float

instance Eq Task where
    (Task p1 _ _ _) == (Task p2 _ _ _) = p1 == p2

instance Ord Task where
    (Task p1 _ _ _) `compare` (Task p2 _ _ _) = p1 `compare` p2

ascendingPriority :: [Task] -> [Task]
ascendingPriority = sortBy (comparing tPriority)

scale :: ScaleFactor -> Task -> Task
scale sf t = t { tComputation = nComp }
    where nComp = tComputation t * sf

responseTimeSingleR :: Task -> Map.Map Task ResponseTime -> Float -> ResponseTime
responseTimeSingleR t rts pr
    | pr > tDeadline t = Nothing
    | pr == r = Just r
    | otherwise = responseTimeSingleR t rts r
    where
        hpts = Map.keys rts
        singleInterference = \hpt -> (tComputation hpt) * ((fromIntegral . ceiling) (pr / tPeriod hpt))
        interference = sum $ map singleInterference hpts
        r = tComputation t + interference

responseTimeSingle :: Task -> Map.Map Task ResponseTime -> ResponseTime
responseTimeSingle t rts
    | rts == Map.empty = Just c
    | failed > 0 = Nothing
    | otherwise = responseTimeSingleR t rts c
    where
        failed = length $ filter isNothing $ Map.elems rts
        c = tComputation t

responseTimesR :: [Task] -> Map.Map Task ResponseTime -> Map.Map Task ResponseTime
responseTimesR [] rts = rts
responseTimesR ts rts = responseTimesR remaining nrts
    where
        remaining = tail ts
        highest = head ts
        nrts = Map.insert highest (responseTimeSingle highest rts) rts

responseTimes :: [Task] -> ScaleFactor -> Map.Map Task ResponseTime
responseTimes [] _  = Map.empty
responseTimes ts sf = responseTimesR (tail tss) rts
    where
        tss = ascendingPriority $ map (scale sf) ts
        highest = head tss
        rts = Map.singleton highest $ responseTimeSingle highest Map.empty

main = do
    print $ responseTimes [Task 1 2 10 10, Task 2 4 6 6, Task 3 6 7 7] 0.5