
module CommunicationAnalysis
    (
        communicationAnalysis,
        routeXY
    )
where

import ResponseTimeAnalysis
import Structures
import Utils

import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.Set as S

type IdLookup = (M.Map Id Task, M.Map Id Core)

core :: TaskId -> TaskMapping -> CoreId
core t tm = fromMaybe (error "Task not in task mapping") $ M.lookup t tm

location :: TaskId -> Application -> Location
location taskId (_, _, tm, cm) = fromMaybe (error "Core not in core mapping") $ M.lookup c cm
    where
        c = core taskId tm

directlyInterferes :: Task -> TrafficFlow -> Task -> TrafficFlow -> Bool
directlyInterferes t tf target targetTf = hp && intersecting
    where
        hp = tPriority t > tPriority target
        intersecting = not . null . intersect tf $ targetTf

directInterferenceSet :: Task -> M.Map Task TrafficFlow -> [Task]
directInterferenceSet t tfs = interfering
    where
        interfering = M.keys . M.filterWithKey (\t2 tf2 -> directlyInterferes t (tFlow t) t2 (tFlow t2)) $ tfs
        tFlow task = fromMaybe (error "Traffic flow not in map") . M.lookup task $ tfs 

indirectInterferenceSet :: Task -> M.Map Task [Task] -> [Task]
indirectInterferenceSet t dis = S.toList . S.fromList . concat . M.elems $ hpts
    where
        hpts = M.filterWithKey (\t2 di -> tPriority t > tPriority t2) dis

routeXY :: Location -> Location -> TrafficFlow
routeXY (ar, ac) (br, bc)
    | (ar, ac) == (br, bc) = []
    | otherwise = cur : routeXY next (br, bc)
    where
        next = case compare ac bc of
            LT -> nextCol succ
            GT -> nextCol pred
            EQ -> case compare ar br of
                LT -> nextRow succ
                GT -> nextRow pred
        cur = ((ar, ac), next) :: Link
        nextCol dir = (ar, dir ac)
        nextRow dir = (dir ar, ac)

route :: Task -> Application -> TrafficFlow
route t a = routeXY sLoc dLoc
    where
        sLoc = lf . tId $ t
        dLoc = lf . cDestination . tCommunication $ t
        lf x = location x a

tasksOnCore :: Core -> Application -> M.Map TaskId Task -> [Task]
tasksOnCore c (_, ts, tm, _) taskLookup = map (toTask . fst) . filter isOnCore . M.toList $ tm
    where
        -- Cleanup
        isOnCore (_, coreId) = coreId == cId c
        toTask task = fromMaybe (error "Task not in task lookup") $ M.lookup task taskLookup

basicNetworkLatency :: Task -> Int -> Platform -> Float
basicNetworkLatency t hops (fs, lb, pd, sf) = (flits * flitBandwidth) + processingDelay
    where
        flits = fi . ceiling $ ((fi . cSize . tCommunication) t /  fi fs)
        flitBandwidth = fi fs / fi lb
        processingDelay = fi hops * (pd / sf)
        fi = fromIntegral

-- Should be returning EndToEndResponseTimes
communicationAnalysis :: Platform -> Application -> M.Map Task Float
communicationAnalysis p@(_, _, _, sf) a@(cs, ts, tm, cm) = basicLatencies
    where
        coreLookup = M.fromList . map (\c -> (cId c, c)) $ cs
        taskLookup = M.fromList . map (\t -> (tId t, t)) $ ts
        idLookup = (taskLookup, coreLookup)
        responseTimes = flattenMap
                      . map (\c -> responseTimeAnalysis (tasksOnCore c a taskLookup) c sf)
                      $ cs
        trafficFlows = expandId taskLookup
                     . M.fromList
                     . map (\t -> (tId t, route t a))
                     $ ts
        basicLatencies = M.fromList
                       . map (\(t, tf) -> (t, basicNetworkLatency t (length tf) p))
                       . M.toList 
                       $ trafficFlows
        -- basicLatencies = (M.fromList . map (\t -> (tId t, basicCommunicationLatency t p (trafficFlows))
        tss = ascendingPriority ts
