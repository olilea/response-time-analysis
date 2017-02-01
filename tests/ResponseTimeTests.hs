
module ResponseTimeTests
    (tests)
where

import qualified Data.Map as M
import Data.Maybe

import Test.Tasty
import Test.Tasty.HUnit

import Internal

defaultComm = Communication 1 1

task1 = Task 1 10 10 1 5 defaultComm
task2 = Task 2 15 8 2 2 defaultComm
task3 = Task 3 10 8 3 4 defaultComm
-- C > D for the following
task4 = Task 4 50 50 4 100 defaultComm 

tests :: TestTree
tests = testGroup "Response time" [unitTests]

unitTests = testGroup "Unit tests" 
    [ testScaling
    , testSingleSolvableFirst
    , testSingleUnsolvableFirst
    , testSingleSolvable
    ]

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