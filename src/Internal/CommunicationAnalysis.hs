
module Internal.CommunicationAnalysis
    (
        communicationAnalysis,
        routeXY
    )
where

import Debug.Trace

import Internal.Structures
import Internal.Utils

import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.Set as S

-- Distinguish between communication and computation response times
type CResponseTime = ResponseTime
type TResponseTime = ResponseTime

type Intermediates = (
    M.Map Task CResponseTime,
    M.Map Task TResponseTime,
    M.Map Task TrafficFlow,
    M.Map Task [Task],
    M.Map Task BasicLatency
    )

core :: TaskId -> TaskMapping -> CoreId
core t tm = fromJust $ M.lookup t tm

location :: TaskId -> Application -> Location
location taskId (_, _, tm, cm) = fromJust $ M.lookup c cm
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
        tFlow task = fromJust . M.lookup task $ tfs 

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

basicNetworkLatency :: Task -> Int -> Platform -> Float
basicNetworkLatency t hops (fs, lb, pd, sf)
    | hops == 0 = 0
    | otherwise = traceShow result result
    where
        flits = fi . ceiling $ ((fi . cSize . tCommunication) t /  fi fs)
        flitBandwidth = fi fs / fi lb
        processingDelay = fi hops * (pd / sf)
        fi = fromIntegral
        result = (flits * flitBandwidth) + processingDelay

analysisR :: Intermediates -> S.Set Task -> Task -> Float -> CResponseTime
analysisR i@(endToEnds, rts, _, dis, bls) indirectTasks t previousTime
    | previousTime > tDeadline t = Nothing
    -- If any tasks that directly interfere with t have no response time
    | any isNothing $ map (fetch rts) taskDi = Nothing
    | any isNothing $ map (fetch endToEnds) taskDi = Nothing
    | previousTime == currentTime = Just currentTime
    | otherwise = analysisR i indirectTasks t currentTime
        where
            taskDi = fetch dis t
            interferenceJitter task
                | task `elem` indirectTasks = fromJust (fetch endToEnds task) - fetch bls task
                | otherwise = 0.0
            interference task = ((previousTime + fromJust (fetch rts task) + interferenceJitter task)
                                / tPeriod task)
                                * fetch bls task
            currentTime = debugOut $ fetch bls t + sum (map interference taskDi)

analysis :: Intermediates -> Task -> CResponseTime
analysis i@(_, rts, _, dis, _) t
    | isNothing (fetch rts t) = Nothing
    | otherwise = analysisR i indirectTasks t 0.0
    where
        taskDi = fetch dis t
        notDirect task = task `notElem` taskDi
        indirectTasks = S.fromList . M.keys
                     -- Only keep tasks that contain indirect interference tasks with t
                     . M.filter (not . null)
                     -- Remove tasks that directly interfere with t
                     . M.map (filter notDirect)
                     -- Interferences for all tasks that interefere with T
                     . M.filterWithKey (\task _ -> elem task taskDi)
                     $ dis
        
communicationAnalysisR :: [Task] -> Intermediates -> M.Map Task CResponseTime
communicationAnalysisR [] (e2es, _, _, _, _) = e2es
communicationAnalysisR (cur:remain) i = communicationAnalysisR remain nextI
    where
        (currentTimes, responseTimes, trafficFlows, dInterferences, latencies) = i
        endToEndTime = analysis i cur
        newTimes = M.insert cur endToEndTime currentTimes
        nextI = (newTimes, responseTimes, trafficFlows, dInterferences, latencies)

-- Should be returning EndToEndResponseTimes
communicationAnalysis :: Platform -> Application -> M.Map Task TResponseTime -> M.Map Task CResponseTime
communicationAnalysis p a@(_, ts, _, _) responseTimes =
    communicationAnalysisR tss (M.empty, responseTimes, trafficFlows, directInterference, basicLatencies)
        where
            taskLookup = M.fromList . map (\t -> (tId t, t)) $ ts
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
