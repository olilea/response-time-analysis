
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

core :: TaskId -> TaskMapping -> CoreId
core t tm = fromMaybe (error "Task not in task mapping") $ M.lookup t tm

location :: TaskId -> Application -> Location
location taskId (_, _, tm, cm) = fromMaybe (error "Core not in core mapping") $ M.lookup c cm
    where
        c = core taskId tm

directInterferenceSet :: Task -> M.Map Task TrafficFlow -> [Task]
directInterferenceSet t tfs = M.keys . M.filter (not . null . intersect tFlow) $ hpts
    where
        tFlow = fromMaybe (error "Traffic flow not in map") $ M.lookup t tfs 
        hpts = M.filterWithKey (\kt _ -> tPriority kt > tPriority t) tfs

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

tasksOnCore :: Core -> Application -> [Task]
tasksOnCore c (cs, ts, tm, _) = map (toTask . fst) . filter isOnCore . M.toList $ tm
    where
        -- Cleanup
        tIdLookup = M.fromList . map (\t -> (tId t, t)) $ ts
        cIdLookup = M.fromList . map (\c -> (cId c, c)) $ cs
        isOnCore (_, coreId) = coreId == cId c
        toTask task = fromMaybe (error "Task not in task lookup") $ M.lookup task tIdLookup
        toCore core = fromMaybe (error "Core not in core lookup") $ M.lookup core cIdLookup

-- Should be returning EndToEndResponseTimes
communicationAnalysis :: Platform -> Application -> TaskResponseTimes
communicationAnalysis p@(fs, lb, sf) a@(cs, ts, tm, cm) = responseTimes
    where
        responseTimes = flattenMap . map (\c -> responseTimeAnalysis (tasksOnCore c a) c sf) $ cs
        rta  = responseTimeAnalysis
        tss = ascendingPriority ts
