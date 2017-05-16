
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map as M

import Data.Maybe
import qualified Data.Random as DR
import Data.Random.Distribution.Uniform
import Data.Word (Word32)

import Debug.Trace

import System.Environment

import Control.Monad
import Control.Monad.Random
import System.Random

import Analysis
import Evolution

type MessageSize = Int

uniformIntRange :: forall m . MonadRandom m => Int -> Int -> m Int
uniformIntRange l u = DR.runRVar (integralUniform l u) (getRandom :: m Word32)

uniformFloatRange :: forall m . MonadRandom m => Float -> Float -> m Float
uniformFloatRange l u = DR.runRVar (floatUniform l u) (getRandom :: m Word32)

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
  msgSize <- getRandomR (128, 32000)
  period <- fromIntegral <$> uniformIntRange 10000000 1000000000
  let computation = targetUtil * period :: Float
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
      tUtil <- uniformFloatRange 0.01 maxTaskUtil
      rTask <- genTask tUtil
      rTasks <- genTaskParams maxTaskUtil (remainingUtil - tUtil)
      return (rTask:rTasks)

genTaskSet :: (MonadRandom m) => Float -> Float -> m [Task]
genTaskSet tmaxTaskUtil targetTotalUtil  =
  genTaskParams tmaxTaskUtil targetTotalUtil >>= assignComms

genCoreSet :: (MonadRandom m) => Int -> (Float, Float) -> m [Core]
genCoreSet rem range@(lower, upper)
  | rem == 0 = return []
  | otherwise = do
      speed <- uniformFloatRange lower upper
      let rc = Core rem speed
      rcs <- genCoreSet (pred rem) range
      return (rc:rcs)

type Arguments = (Int, Float, Float)

extractArguments :: IO Arguments
extractArguments = do
  as <- getArgs
  case as of
    [] -> error "No NOC size or util given"
    (nocSize:maxUtil:taskSetUtil:[]) ->
      return $ (read nocSize :: Int, read maxUtil :: Float, read taskSetUtil :: Float)

runs :: Int -> String -> IO ()
runs nocSize dataset = do

  -- mapM (\i -> gaRun p nocSize ep (suffix i)) [1..10]
  mapM (\i -> ccgaRun p nocSize cep (suffix i)) [1..10]
  return ()
    where
        ep = EvolutionParameters 100 200 2 0.7 0.01
        cep = CCEvolutionParameters 100 200 2 0.7 0.01 10
        p = Platform 1.0 1.0 1.0
        nocSizeS = show nocSize
        suffix i = dataset ++ "_" ++ nocSizeS ++ "x" ++ nocSizeS ++ "_" ++ (show i)

main :: IO ()
main = do
  g <- getStdGen
  mapM (\i -> runs i "ava_HET_S") [3]
  return ()

statsToCsv :: [Stat] -> String
statsToCsv ss = unlines $ (:) header $ reverse (map statToCsv ss)
  where
    header = "Generation,Breakdown Frequency,Schedulability"

statToCsv :: Stat -> String
statToCsv s = (show gen) ++ "," ++ (show bdf) ++ "," ++ (show sched)
  where
    (Stat gen bdf sched) = s

ccgaRun :: Platform
        -> Int
        -> CCEvolutionParameters
        -> String
        -> IO ()
ccgaRun p nocSize ep suffix = do
  g <- newStdGen
  -- (nocSize, maxTaskUtil, taskSetUtil) <- extractArguments
  -- ts <- genTaskSet maxTaskUtil taskSetUtil
  let ts = map (\((id, c, t), comm) -> (Task id (t * 1000000000) (t * 1000000000) (c * 1000000000) comm))
         $ zip avaTs avaCs
  -- let cs = [Core idee 1.0 | idee <- [1..nocSize*nocSize]]
  cs <- reverse <$> genCoreSet (nocSize*nocSize) (0.2, 1.8)
  let coreMapping = M.fromList $ zip [1..nocSize*nocSize] [Location r c | r <- [1..nocSize], c <- [1..nocSize]]
  let d = Domain cs ts p
  putStrLn $ show . sum . map cSpeed $ cs
  putStrLn . show $ length ts

  let met pm tm = let missing = fromIntegral $ missingDeadlines p (Application cs ts (M.fromList tm) coreMapping (M.fromList pm)) 1.0 in
        (-) 100.0 $ (fromIntegral missing) / (fromIntegral (length ts)) * 100.0
  let bdf pm tm = let bFreq = bdf2 p (Application cs ts (M.fromList tm) coreMapping (M.fromList pm)) in
        case bFreq of
          Nothing -> 1000.0
          Just f -> f
  let (mapping, stats) = runCCGA g ep d bdf met
  writeFile ("data/ccga_" ++ suffix ++ ".csv") $ statsToCsv stats

avaTs = [ (1, 0.005, 0.5)
        , (2, 0.005, 0.1)
        , (3, 0.005, 0.1)
        , (4, 0.005, 0.5)
        , (5, 0.005, 0.1)
        , (6, 0.01, 0.4)
        , (7, 0.01, 0.4)
        , (8, 0.01, 0.4)
        , (9, 0.01, 0.4)
        , (10, 0.01, 0.4)
        , (11, 0.01, 0.4)
        , (12, 0.01, 0.4)
        , (13, 0.01, 0.4)
        , (14, 0.01, 1.0)
        , (15, 0.001, 0.01)
        , (16, 0.001, 0.01)
        , (17, 0.15, 0.5)
        , (18, 0.02, 0.04)
        , (19, 0.02, 0.04)
        , (20, 0.02, 0.04)
        , (21, 0.02, 0.04)
        , (22, 0.02, 0.04)
        , (23, 0.02, 0.04)
        , (24, 0.01, 0.04)
        , (25, 0.01, 0.04)
        , (26, 0.01, 0.4)
        , (27, 0.01, 0.4)
        , (28, 0.03, 0.04)
        , (29, 0.001, 0.01)
        , (30, 0.02, 0.04)
        , (31, 0.02, 0.04)
        , (32, 0.02, 1.0)
        , (33, 0.01, 0.5)
        ]

avaCs :: [Communication]
avaCs = [ Communication 14 2048
     , Communication 14 512
     , Communication 14 1024
     , Communication 32 1024
     , Communication 32 1024
     , Communication 18 38400
     , Communication 19 38400
     , Communication 20 38400
     , Communication 21 38400
     , Communication 22 38400
     , Communication 23 38400
     , Communication 24 38400
     , Communication 25 38400
     , Communication 15 2048
     , Communication 30 38400
     , Communication 31 38400
     , Communication 32 32768
     , Communication 26 2048
     , Communication 26 2048
     , Communication 26 2048
     , Communication 26 2048
     , Communication 27 2048
     , Communication 27 2048
     , Communication 27 2048
     , Communication 27 2048
     , Communication 28 8192
     , Communication 28 8192
     , Communication 32 4096
     , Communication 33 16384
     , Communication 33 512
     , Communication 33 512
     , Communication 17 4096
     , Communication 17 2048
    ]
