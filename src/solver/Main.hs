
module Main where

import qualified Data.Map as M
import Data.Maybe

import Control.Monad
import Control.Monad.Random
import System.Random

import Analysis
import Coevolve

type MessageSize = Int

pick :: (MonadRandom m) => [a] -> m a
pick as = do
  index <- getRandomR (0, length as - 1)
  return $ as !! index

cpuUtilisation :: [Core] -> [Task] -> Float
cpuUtilisation cs ts = used / available
  where
    available = sum . map cSpeed $ cs
    used = sum . map (\t -> (tComputation t / tPeriod t)) $ ts

genTask :: (MonadRandom m) => Float -> m (TaskPeriod, TaskDeadline, TaskComputation, MessageSize)
genTask targetUtil = do
  msgSize <- getRandomR (1, 2)
  period <- getRandomR (1.0, 100.0)
  let computation = targetUtil * period
  deadline <- getRandomR (computation, period)
  -- Should be using deadline!
  return $ (period, period, computation, msgSize)

assignComms :: (MonadRandom m)
            => [(TaskPeriod, TaskDeadline, TaskComputation, MessageSize)]
            -> m [Task]
assignComms tParams = do
  let destPicker id = pick [x | x <- [1..length tParams], x /= id]
  dests <- mapM destPicker [1..length tParams]
  return $ map (\((p, d, c, msg), (id, dest)) -> Task id p d c (Communication dest msg))
         $ zip tParams 
         $ zip [1..length tParams] dests

genTaskParams :: (MonadRandom m)
            => Float
            ->  m [(TaskPeriod, TaskDeadline, TaskComputation, MessageSize)]
genTaskParams remainingUtil
  | remainingUtil <= 0.0 = return []
  | otherwise = do
      tUtil <- getRandomR (0.0, 0.75)
      rTask <- genTask tUtil
      rTasks <- genTaskParams (remainingUtil - tUtil)
      return (rTask:rTasks)

genTaskSet :: (MonadRandom m) => Float -> m [Task]
genTaskSet targetUtil = genTaskParams targetUtil >>= assignComms

-- TODO: Need to work out why some things have the same priority.
-- TODO: Should add a hall of fame and elitism
-- TODO: Why do the priority mappings end up looking like that task Id should be the priority?
-- Basically, fix the priority operators
-- TODO: Fix BF function - if on the last scaling it finds it misses a deadline, it will
--   return a Nothing, even though it might have been succeeding below the 1.0 mark.
--   Basically should be returning the last successful scale.
main :: IO ()
main = do
  g <- getStdGen
  ts <- genTaskSet 5.0
  putStrLn . show $ length ts
  putStrLn . show $ ts
  let d = Domain cs ts p

  let end d pm tm = endToEnd p sf (Application cs ts (M.fromList tm) coreMapping (M.fromList pm))
  let met d pm tm = fromIntegral $ missingDeadlines p sf (Application cs ts (M.fromList tm) coreMapping (M.fromList pm))
  let bdf d pm tm = let r = breakdownFrequency p (Application cs ts (M.fromList tm) coreMapping (M.fromList pm)) 50 in
        case r of
          Nothing -> ((met d pm tm) + 1) * 10.0 -- Fix this line (see fix BF todo)
          Just s -> s
  let mappings = runGA g ep d bdf
  putStrLn . show $ mappings
  putStrLn . show $ bdf d (fst mappings) (snd mappings)
  putStrLn . show $ met d (fst mappings) (snd mappings)
  putStrLn . show $ end d (fst mappings) (snd mappings)
    where
        ep = EvolutionParameters 100 50 10 0.7 0.05
        sf = 1.0
        cs = [Core idee 1.0 | idee <- [1..9]]
        coreMapping = M.fromList $ zip [1..9] [Location r c | r <- [1..3], c <- [1..3]]
        p = Platform 1.0 1.0 1.0

