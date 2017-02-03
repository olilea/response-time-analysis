
module CommunicationTests
    (tests)
where

import qualified Data.Map as M
import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit

import Analysis.Internal

cores :: [Core]
cores = [Core idee 1.0 | idee <- [1..9]]

tasks :: [Task]
tasks = [Task idee 20.0 20.0 prior 1.0 (Communication (destination idee) 5)
        | idee <- [1..5], prior <- [1..5]]
    where
        destination tIdee = fromJust $ M.lookup tIdee destLookup
        destLookup = M.fromList
            [ (1, 4)
            , (2, 4)
            , (3, 1)
            , (4, 3)
            , (5, 2)
            ]

platform :: Platform
platform = Platform 1.0 1.0 1.0

application :: Application
application = Application cores tasks taskMapping coreMapping

coreMapping :: CoreMapping
coreMapping = M.fromList $ zip [1..9] [Location r c | r <- [1..3], c <- [1..3]]

taskMapping :: TaskMapping
taskMapping = M.fromList
    [ (1, 4)
    , (2, 8)
    , (3, 9)
    , (4, 1)
    , (5, 1)
    ]

tests :: TestTree
tests = testGroup "Communication time" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ directInterferenceTests
    , routingTests
    , latencyTests
    , analysisTests
    ]

directInterferenceTests = testGroup "Direct interference" []

routingTests = testGroup "Routing" [xyRoutingTests, taskRoutingTests]

latencyTests = testGroup "Basic network latency" []

analysisTests = testGroup "Communication analysis" 
    [ singleCommTimeTests
    , overallCommTimeTests
    ]

xyRoutingTests = testGroup "XY routing" []
taskRoutingTests = testGroup "Task routing" []

singleCommTimeTests = testGroup "Single communication analysis" []
overallCommTimeTests = testGroup "Overall communication analysis" []



