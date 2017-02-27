
module Main where

import qualified Data.Map as M
import Data.Maybe

import Debug.Trace

import System.Environment

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
  period <- getRandomR (1.0, 300.0)
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
            -> Float
            ->  m [(TaskPeriod, TaskDeadline, TaskComputation, MessageSize)]
genTaskParams maxTaskUtil remainingUtil
  | remainingUtil <= 0.0 = return []
  | otherwise = do
      tUtil <- getRandomR (0.0, maxTaskUtil)
      rTask <- genTask tUtil
      rTasks <- genTaskParams maxTaskUtil (remainingUtil - tUtil)
      return (rTask:rTasks)

genTaskSet :: (MonadRandom m) => Float -> Float -> m [Task]
genTaskSet tmaxTaskUtil targetTotalUtil  =
  genTaskParams tmaxTaskUtil targetTotalUtil >>= assignComms

type Arguments = (Int, Float, Float)

extractArguments :: IO Arguments
extractArguments = do
  as <- getArgs
  case as of
    [] -> error "Not NOC size or util given"
    (nocSize:maxUtil:taskSetUtil:[]) ->
      return $ (read nocSize :: Int, read maxUtil :: Float, read taskSetUtil :: Float)

-- TODO: Should add a hall of fame and elitism
-- TODO: Increase diversity in population and stop it converging on
--   local minima
main :: IO ()
main = do
  g <- getStdGen
  (nocSize, maxTaskUtil, taskSetUtil) <- extractArguments

  let cs = [Core idee 1.0 | idee <- [1..nocSize*nocSize]]
  let coreMapping = M.fromList $ zip [1..nocSize*nocSize] [Location r c | r <- [1..nocSize], c <- [1..nocSize]]
  ts <- genTaskSet maxTaskUtil taskSetUtil
  let d = Domain cs ts p
  putStrLn . show $ length ts

  let end d pm tm = endToEnd p (Application cs ts (M.fromList tm) coreMapping (M.fromList pm)) sf
  let met d pm tm = fromIntegral $ missingDeadlines p (Application cs ts (M.fromList tm) coreMapping (M.fromList pm)) sf
  let bdf d pm tm = let bFreq = bdf2 p (Application cs ts (M.fromList tm) coreMapping (M.fromList pm)) in
        case bFreq of
          Nothing -> 1000.0
          Just f -> f

  let mappings = runGA g ep d bdf

  putStrLn . show $ mappings
  putStrLn . show $ bdf d (fst mappings) (snd mappings)
  putStrLn . show $ met d (fst mappings) (snd mappings)
  putStrLn . show $ end d (fst mappings) (snd mappings)
    where
        ep = EvolutionParameters 40 100 2 0.7 0.1 10
        sf = 1.0
        p = Platform 1.0 1.0 1.0

