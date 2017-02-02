
import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import qualified ResponseTimeTests as RTT
import qualified CommunicationTests as CT

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [RTT.tests, CT.tests]