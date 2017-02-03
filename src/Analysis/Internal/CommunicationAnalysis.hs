
module Analysis.Internal.CommunicationAnalysis
    (
        communicationAnalysis,
        routeXY
    )
where

import Analysis.Internal.Structures
import Analysis.Internal.Utils

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
location taskId (Application _ _ tm cm) = fromJust $ M.lookup c cm
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
routeXY l1@(Location ar ac) l2@(Location br bc)
    | (ar, ac) == (br, bc) = []
    | otherwise = cur : routeXY next l2
    where
        next = case compare ac bc of
            LT -> nextCol succ
            GT -> nextCol pred
            EQ -> case compare ar br of
                LT -> nextRow succ
                GT -> nextRow pred
        cur = Link l1 next :: Link
        nextCol dir = Location ar (dir ac)
        nextRow dir = Location (dir ar) ac

route :: Task -> Application -> TrafficFlow
route t a = routeXY sLoc dLoc
    where
        sLoc = lf . tId $ t
        dLoc = lf . cDestination . tCommunication $ t
        lf x = location x a

basicNetworkLatency :: Platform -> Task -> Int -> Float
basicNetworkLatency (Platform fs ld rd) t hops
    | hops == 0 = 0
    | otherwise = linkDelays + routerDelays + otherFlitLinkDelays
        where
            links = fromIntegral hops
            routers = fromIntegral hops - 1
            linkDelays = links * ld
            routerDelays = routers * rd
            flits :: Float
            flits = (fromIntegral . cSize . tCommunication) t / fs
            otherFlitLinkDelays = flits * ld

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

scalePlatform :: Platform -> ScaleFactor -> Platform
scalePlatform (Platform fs ld rd) sf = Platform fs (ld * sf) (rd * sf)

-- Should be returning EndToEndResponseTimes
communicationAnalysis :: Platform 
                      -> ScaleFactor
                      -> Application 
                      -> M.Map Task TResponseTime 
                      -> M.Map Task CResponseTime
communicationAnalysis p sf a@(Application _ ts _ _) responseTimes =
    communicationAnalysisR tss (M.empty, responseTimes, trafficFlows, directInterference, basicLatencies)
        where
            scaledPlatform = scalePlatform p sf
            taskLookup = M.fromList . map (\t -> (tId t, t)) $ ts
            trafficFlows = expandId taskLookup
                         . M.fromList
                         . map (\t -> (tId t, route t a))
                         $ ts
            directInterference = M.fromList
                         . map (\t -> (t, directInterferenceSet t trafficFlows))
                         $ ts
            basicLatencies = M.fromList
                         . map (\(t, tf) -> (t, basicNetworkLatency scaledPlatform t (length tf)))
                         . M.toList 
                         $ trafficFlows
            tss = ascendingPriority ts
