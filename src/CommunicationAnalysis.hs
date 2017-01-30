
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

type Intermediates = (
    EndToEndResponseTimes,
    TaskResponseTimes,
    M.Map Task TrafficFlow,
    M.Map Task [Task],
    M.Map Task Float
    )

core :: TaskId -> TaskMapping -> CoreId
core t tm = fromMaybe (error "Task not in task mapping") $ M.lookup t tm

location :: TaskId -> Application -> Location
location taskId (_, _, tm, cm) = fromMaybe (error "Core not in core mapping") $ M.lookup c cm
    where
        c = core taskId tm

directlyInterferes :: Task -> TrafficFlow -> Task -> TrafficFlow -> Bool
directlyInterferes t tf target targetTf = hp && intersecting
    where
        hp = tPriority t < tPriority target
        intersecting = not . null . intersect tf $ targetTf

directInterferenceSet :: Task -> M.Map Task TrafficFlow -> [Task]
directInterferenceSet t tfs = interfering
    where
        interfering = M.keys . M.filterWithKey (\t2 _ -> directlyInterferes t (tFlow t) t2 (tFlow t2)) $ tfs
        tFlow task = fromMaybe (error "Traffic flow not in map") . M.lookup task $ tfs 

-- indirectInterferenceSet :: Task -> M.Map Task [Task] -> [Task]
-- indirectInterferenceSet t dis = distinct . concat . M.elems  $ hpts
--     where
--         di = fromMaybe (error "Task not in map") . M.lookup t dis
--         hpts = M.filterWithKey (\t2 di2 -> tPriority t > tPriority t2)
--         distinct = S.toList . S.fromList

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

analysis :: Intermediates -> Task -> EndToEndResponseTime
analysis (endToEnds, rts, tfs, dis, bls) t = Nothing
    where
        taskRt = fetch rts t
        taskTf = fetch tfs t
        taskDi = fetch dis t
        taskBl = fetch bls t
        notDirect task = notElem task taskDi
        taskIndirect = M.keys
                     -- Only keep tasks that contain indirect interference tasks with t
                     . M.filter (not . null)
                     -- Remove tasks that directly interfere with t
                     . M.map (\is -> filter notDirect is)
                     -- Interferences for all tasks that interefere with T
                     . M.filterWithKey (\task _ -> elem task taskDi)
                     $ dis
        
communicationAnalysisR :: [Task] -> Intermediates -> EndToEndResponseTimes
communicationAnalysisR [] (e2es, _, _, _, _) = e2es
communicationAnalysisR (cur:remain) i = communicationAnalysisR remain nextI
    where
        (currentTimes, responseTimes, trafficFlows, dInterferences, latencies) = i
        endToEndTime = analysis i cur
        newTimes = M.insert cur endToEndTime currentTimes
        nextI = (newTimes, responseTimes, trafficFlows, dInterferences, latencies)

-- Should be returning EndToEndResponseTimes
communicationAnalysis :: Platform -> Application -> EndToEndResponseTimes
communicationAnalysis p@(_, _, _, sf) a@(cs, ts, tm, cm) =
    communicationAnalysisR tss (M.empty, responseTimes, trafficFlows, directInterference, basicLatencies)
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
            directInterference = M.fromList
                         . map (\t -> (t, directInterferenceSet t trafficFlows))
                         $ ts
            basicLatencies = M.fromList
                         . map (\(t, tf) -> (t, basicNetworkLatency t (length tf) p))
                         . M.toList 
                         $ trafficFlows
            tss = ascendingPriority ts
