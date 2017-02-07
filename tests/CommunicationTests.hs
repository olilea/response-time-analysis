
module CommunicationTests
    
where

import qualified Data.Map as M
import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit

import Analysis.Internal


{-
    Following produces 5 tasks, with the following communications:
        1 -> 4,
        2 -> 4,
        3 -> 1,
        4 -> 3,
        5 -> 2,
        6 -> 2
    Direct interference sets are:
        1: {},
        2: {1},
        3: {2},
        4: {},
        5: {4}
        6: {}.
-}

cores :: [Core]
cores = [Core idee 1.0 | idee <- [1..9]]

tasks :: [Task]
tasks = [Task idee 20.0 20.0 1.0 (Communication (destination idee) 5)
        | idee <- [1..6]]
    where
        destination tIdee = fromJust $ M.lookup tIdee destLookup
        destLookup = M.fromList
            [ (1, 4)
            , (2, 4)
            , (3, 1)
            , (4, 3)
            , (5, 2)
            , (6, 2)
            ]

platform :: Platform
platform = Platform 1.0 1.0 1.0

application :: Application
application = Application cores tasks taskMapping coreMapping priorityMapping

coreMapping :: CoreMapping
coreMapping = M.fromList $ zip [1..9] [Location r c | r <- [1..3], c <- [1..3]]

taskMapping :: TaskMapping
taskMapping = M.fromList
    [ (1, 4)
    , (2, 8)
    , (3, 9)
    , (4, 1)
    , (5, 1)
    , (6, 8)
    ]

priorityMapping = M.fromList [(x, x) | x <- [1..length tasks]]

tests :: TestTree
tests = testGroup "Communication time" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ routingTests
    , directInterferenceTests
    , latencyTests
    , analysisTests
    ]

routingTests = testGroup "Routing" [xyRoutingTests, taskRoutingTests]

xyRoutingTests = testGroup "XY routing"
    [ testXyDestIsSource
    , testXyFarAway
    , testXyGoingUp
    , testXyGoingDown
    , testXyGoingLeft
    , testXyGoingRight
    ]

taskRoutingTests = testGroup "Task routing"
    [ testTaskRouteWhenDestIsSame
    , testTaskRouteWhenDestNotSource
    ]

directInterferenceTests = testGroup "Direct interference"
    [ testNoInterference tfMap
    , testInterference tfMap
    ]
        where
            tfMap = M.fromList . map (\t -> (t, route t application)) $ tasks

latencyTests = testGroup "Basic network latency"
    [ testLantecyDestIsSource
    , testLatencySingleHop
    , testLatencyMultiHop]

analysisTests = testGroup "Communication analysis" 
    [ testCommunicationAnalysis ]

-- TESTS

-- XY Routing

testXyDestIsSource = testCase "When dest is same as source" $
    routeXY (Location 2 2) (Location 2 2) @?= []

testXyFarAway = testCase "When the dest is far away from the source" $
    routeXY (Location 1 1) (Location 3 3) @?= expected
        where
            expected = [ Link (Location 1 1) (Location 1 2)
                       , Link (Location 1 2) (Location 1 3)
                       , Link (Location 1 3) (Location 2 3)
                       , Link (Location 2 3) (Location 3 3)
                       ]

testXyGoingUp = testCase "When dest is directly above source" $
    routeXY (Location 3 1) (Location 1 1) @?= expected
        where
            expected = [ Link (Location 3 1) (Location 2 1)
                       , Link (Location 2 1) (Location 1 1)
                       ]

testXyGoingDown = testCase "When dest is directly below source" $
    routeXY (Location 1 1) (Location 3 1) @?= expected
        where
            expected = [ Link (Location 1 1) (Location 2 1)
                       , Link (Location 2 1) (Location 3 1)
                       ]

testXyGoingLeft = testCase "When dest is directly left of source" $
    routeXY (Location 1 3) (Location 1 1) @?= expected
        where
            expected = [ Link (Location 1 3) (Location 1 2)
                       , Link (Location 1 2) (Location 1 1)
                       ]

testXyGoingRight = testCase "When dest is directly right of source" $
    routeXY (Location 1 1) (Location 1 3) @?= expected
        where
            expected = [ Link (Location 1 1) (Location 1 2)
                       , Link (Location 1 2) (Location 1 3)
                       ]

-- Task routing

testTaskRouteWhenDestIsSame = testCase "When the destination is the same as source" $
    route (tasks !! 5) application @?= []

testTaskRouteWhenDestNotSource = testCase "When destination is different to source" $
    route (tasks !! 2) application @?= expected
        where
            expected = [ Link (Location 3 3) (Location 3 2)
                       , Link (Location 3 2) (Location 3 1)
                       , Link (Location 3 1) (Location 2 1)
                       ]

-- Direct interference

testNoInterference tfMap = testCase "When there is none" $
    directInterferenceSet (tasks !! 3) priorityMapping tfMap @?= []

testInterference tfMap = testCase "When there is direct interference" $
    directInterferenceSet (tasks !! 2) priorityMapping tfMap @?= [tasks !! 1]

-- Basic latency

testLantecyDestIsSource = testCase "When destination is the same as the source" $
    basicNetworkLatency platform (tasks !! 5) 0 @?= 0.0

testLatencySingleHop = testCase "When destination is one hop away from the source" $
    basicNetworkLatency platform (tasks !! 0) 1 @?= 6.0

testLatencyMultiHop = testCase "When destination is multiple hops away from the source" $
    basicNetworkLatency platform (tasks !! 2) 3 @?= 10.0

-- Communication analysis
testCommunicationAnalysis = testCase "When the tasks are all schedulable" $
    communicationAnalysis platform 1.0 application rts @?= M.empty
        where
            rts = M.empty