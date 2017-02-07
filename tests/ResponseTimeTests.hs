
module ResponseTimeTests
    (tests)
where

import qualified Data.Map as M
import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit

import Analysis.Internal

defaultComm = Communication 1 1
defaultScale = 1.0

core = Core 1 1.0

task1 = Task 1 10 10 5 defaultComm
task2 = Task 2 15 8 2 defaultComm
task3Unsch = Task 3 10 8 4 defaultComm
task3Sch = Task 3 20 20 4 defaultComm
-- C > D for the following
task4 = Task 4 50 50 100 defaultComm 

priorityMapping = M.fromList [(x, x) | x <- [1..4]]

tests :: TestTree
tests = testGroup "Response time" [units, properties]

units :: TestTree
units = testGroup "Unit tests" 
    [ scalingTests
    , singleResponseTimeTests
    , allResponseTimeTests
    ]

properties :: TestTree
properties = testGroup "Properties" []

scalingTests = testGroup "Scaling" [testScaling]

singleResponseTimeTests = testGroup "Single response time"
    [ testSingleSolvableFirst
    , testSingleUnsolvableFirst
    , testSingleSolvable
    , testSingleUnsolvable
    , testSingleHigherMissed
    ]

allResponseTimeTests = testGroup "Overall response times"
    [ testAllNoTasks
    , testAllSolvable
    , testAllUnsolvable
    , testAllScaling
    , tsetAllScalingMakeSolvable
    ]

schedulable :: M.Map Task ResponseTime -> Bool
schedulable = all isJust . M.elems

testScaling = testCase "Should scale a task's computation time" $
    scale 0.5 task1 @?= task1 {tComputation = 1.5}

testSingleSolvableFirst = testCase "Should return C for the first task's response time if C < D" $
    responseTimeSingle task1 M.empty @?= (Just . tComputation) task1

testSingleUnsolvableFirst = testCase "Should return Nothing for the first task if C > D" $
    responseTimeSingle task4 M.empty @?= Nothing

testSingleSolvable = testCase "Should deduce the response time task suffering interference" $
    responseTimeSingle task2 responseMap @?= Just answer
        where
            responseMap = M.fromList [(task1, (Just . tComputation) task1)]
            answer = tComputation task1 + tComputation task2

testSingleUnsolvable = testCase "Should return Nothing when a task misses deadline due to interference" $
    responseTimeSingle task3Unsch responseMap @?= Nothing
        where
            responseMap = M.fromList [(task1, (Just . tComputation) task1)]

testSingleHigherMissed = testCase "Should return Nothing when a higher priority task missed deadline" $
    responseTimeSingle task2 responseMap @?= Nothing
        where
            responseMap = M.fromList [(task1, Nothing)]

testAllNoTasks = testCase "Should return the empty map when there are no input tasks" $
    responseTimeAnalysis [] priorityMapping core defaultScale @?= M.empty

testAllSolvable = testCase "Should return a map of response times for a schedulable taskset" $ do
    schedulable responses @?= True
    responses @?= expected
        where
            responses = responseTimeAnalysis [task1, task2, task3Sch] priorityMapping core defaultScale
            expected = M.fromList [(task1, (Just . tComputation) task1)
                                , (task2, Just (tComputation task1 + tComputation task2))
                                , (task3Sch, Just 18)
                                ]

testAllUnsolvable = testCase "Should return a map of response times containing Nothing for an unschedulable taskset" $ do
    schedulable responses @?= False
    responses @?= expected
        where
            responses = responseTimeAnalysis [task1, task2, task3Unsch] priorityMapping core defaultScale
            expected = M.fromList [(task1, (Just . tComputation) task1)
                                  , (task2, Just (tComputation task1 + tComputation task2))
                                  , (task3Unsch, Nothing)
                                  ]

testAllScaling = testCase "Scaling down computation times should maintain a schedulable task set's schedulability" $ do
    schedulable before @?= True
    schedulable after @?= True
    after @?= expected
        where
            sf = 0.5
            tasks = [task1, task2, task3Sch]
            before = responseTimeAnalysis tasks priorityMapping core defaultScale
            after = responseTimeAnalysis tasks priorityMapping core sf
            scaledTasks = map (scale sf) tasks
            times = map (Just . (* sf)) [tComputation task1
                                        , tComputation task1 + tComputation task2
                                        , tComputation task1 + tComputation task2 + tComputation task3Sch
                                        ]
            expected = M.fromList $ zip scaledTasks times

tsetAllScalingMakeSolvable = testCase "Scaling down computation times can make unschedulable task set schedulable" $ do
    schedulable before @?= False
    schedulable after @?= True
    after @?= expected
        where
            sf = 0.5
            tasks = [task1, task2, task3Unsch]
            before = responseTimeAnalysis tasks priorityMapping core 1.0
            after = responseTimeAnalysis tasks priorityMapping core sf
            scaledTasks = map (scale sf) tasks
            times = map (Just . (* sf)) [
                    tComputation task1
                    , tComputation task1 + tComputation task2
                    , tComputation task1 + tComputation task2 + tComputation task3Unsch
                    ]
            expected = M.fromList $ zip scaledTasks times