{-# LANGUAGE ScopedTypeVariables #-}

module Evolution.Coevolve
  ( runCCGA
  )
where

import Analysis


import Evolution.Internal.Structures
import Evolution.Internal.Utils

import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M

import System.Random
import System.Random.Shuffle

import Debug.Trace

type HallOfFame = (TPMap, CPMap, TMap, Fitness)

type Population = ([TPMap], [CPMap], [TMap])
type FitnessF = (TPMap -> CPMap -> TMap -> Fitness)

-- Choosing representatives

represent :: (MonadRandom m)
          => Int
          -> [(a, Fitness)]
          -> m [a]
represent poolSize xs = do
  rs <- replicateM (poolSize - 1) (pick inds)
  return $ best:rs
  where
    inds = map fst xs
    best = fst . head . sortBy (comparing snd) $ xs


-- Generating the initial population

-- Generate random priority mappings and task mappings
genPopulation :: (MonadRandom m)
               => Int
               -> Int
               -> Int
               -> m ([TPMap], [CPMap], [TMap])
genPopulation popSize cs ts = do
    let populate = replicateM popSize
    tps <- populate $ genPriorityMapping ts
    cps <- populate $ genPriorityMapping ts
    ms <- populate $ genTaskMapping ts cs
    return (tps, cps, ms)

-- Initial fitness calculation

-- Match a priority mapping to a random task mapping
-- in order to assign a fitness
initialTaskPriorityFitness :: (MonadRandom m)
                           => FitnessF
                           -> Population
                           -> m [(TPMap, Fitness)]
initialTaskPriorityFitness fnf (tps, cps, ms) = do
  mPicks <- replicateM (length ms) (pick ms)
  cpPicks <- replicateM (length cps) (pick cps)
  -- Take a [PMap] and produce a [(Priorities, Fitness)], sorted
  return
    . sortBy (comparing snd)
    . map (\(tp, cp, m) -> (tp, fnf tp cp m))
    $ zip3 tps cpPicks mPicks

initialCommPriorityFitness :: (MonadRandom m)
                           => FitnessF
                           -> Population
                           -> m [(CPMap, Fitness)]
initialCommPriorityFitness fnf (tps, cps, ms) = do
  mPicks <- replicateM (length ms) (pick ms)
  tpPicks <- replicateM (length tps) (pick tps)
  return
    . sortBy (comparing snd)
    . map (\(tp, cp, m) -> (cp, fnf tp cp m))
    $ zip3 tpPicks cps mPicks

-- Match a task mapping to a random priority mapping
-- in order to assign a fitness
initialTaskMapFitness :: (MonadRandom m)
                      => FitnessF
                      -> Population
                      -> m [(TMap, Float)]
initialTaskMapFitness fnf (tps, cps, ms) = do
  tpPicks <- replicateM (length tps) (pick tps)
  cpPicks <- replicateM (length cps) (pick cps)
  return
    . sortBy (comparing snd)
    . map (\(tp, cp, m) -> (m, fnf tp cp m))
    $ zip3 tpPicks cpPicks ms

-- Actual GA implementation

runCCGA :: (RandomGen g)
         => g
         -> CCEvolutionParameters
         -> Domain
         -> FitnessF
         -> (TPMap -> CPMap -> TMap -> Float)
         -> ((TPMap, CPMap, TMap), [Stat])
runCCGA g ep d@(Domain cs ts p) fnf schedf = evalRand run g
  where
    run = do
        initialPop <- zeroGen popSize d fnf
        let bestTPm = getBest . fst3 $ initialPop
        let bestCPm = getBest . snd3 $ initialPop
        let bestTm = getBest . third $ initialPop
        let initialHof@(bestTPs, bestCPs, bestMs, fitness) = (bestTPm, bestCPm, bestTm, fnf bestTPm bestCPm bestTm)
        let stats = [(Stat 0 fitness (schedf bestTPs bestCPs bestMs))]
        (finalHof@(tpm, cpm, tm, _), stats) <- runCCGA' ep d fnf schedf initialHof initialPop stats 1
        return ((tpm, cpm, tm), stats)
    popSize = cePopulationSize ep
    getBest = fst . head . sortBy (comparing snd)


-- Create an initial population and assign them each a fitness.
-- This fitness is the result of a random pairing across the
-- subpopulations.
-- This fitness is required in order to provide the respresentatives
-- for the initial population
zeroGen :: (MonadRandom m)
        => Int
        -> Domain
        -> FitnessF
        -> m ([(TPMap, Fitness)], [(CPMap, Fitness)], [(TMap, Fitness)])
zeroGen popSize d@(Domain cs ts _) fnf = do
  (tps, cps, ms) <- genPopulation popSize (length cs) (length ts)
  (,,) <$> initialTaskPriorityFitness fnf (tps, cps, ms)
      <*> initialCommPriorityFitness fnf (tps, cps, ms)
      <*> initialTaskMapFitness fnf (tps, cps, ms)

-- The recursive part of the GA.
--
-- Firstly, select from the population.
--
-- Produce variation in the population through use of crossover and
-- mutation operatiions:
-- Crossover involves selecting two random individuals and a
-- random number from 0-1. If this number is <= the crossover rate
-- then apply the operator. Repeat until enough for next generation.
-- Mutation involves selecting a number from 0-1. If this number <= the
-- mutation rate then mutate that individual.
--
-- Then calculate the fitness of each of the individuals in the new
-- population by combining them with the given represntatives.
runCCGA' :: (MonadRandom m)
          => CCEvolutionParameters
          -> Domain
          -> FitnessF
          -> (TPMap -> CPMap -> TMap -> Float)
          -> HallOfFame
          -> ([(TPMap, Fitness)], [(CPMap, Fitness)], [(TMap, Fitness)])
          -> [Stat]
          -> Int
          -> m (HallOfFame, [Stat])
runCCGA' ep@(CCEvolutionParameters gens _ _ cxPb mtPb poolSize) dom fnf schedf hof pop@(tps, cps, ms) stats genNumber
  | genNumber > gens = traceShow ms $ return (hof, stats)
  | otherwise = do
      tpReps <- represent poolSize tps
      cpReps <- represent poolSize cps
      mReps <- represent poolSize ms
      (tps', cps', ms', hof'@(bestTPs, bestCPs, bestMs, bestFit)) <- evolve ep dom fnf hof (tpReps, cpReps, mReps) pop
      let pop' = (sortBy (comparing snd) tps', sortBy (comparing snd) cps', sortBy (comparing snd) ms')
      let stats' = (Stat genNumber bestFit (schedf bestTPs bestCPs bestMs)) : stats
      traceShow bestFit $ runCCGA' ep dom fnf schedf hof' pop' stats' (genNumber + 1)

-- Evolve a population into the next generation using the provided
-- fitness function and parameters
evolve :: forall m . (MonadRandom m)
       => CCEvolutionParameters
       -> Domain
       -> FitnessF
       -> HallOfFame
       -> ([TPMap], [CPMap], [TMap])
       -> ([(TPMap, Fitness)], [(CPMap, Fitness)], [(TMap, Fitness)])
       -> m ([(TPMap, Fitness)], [(CPMap, Fitness)], [(TMap, Fitness)], HallOfFame)
evolve ep dom fnf hof@(hofTPm, hofCPm, hofM, hofFit) reps@(tpReps, cpReps, mReps) pop@(tps, cps, ms) = do
  selectedTPs <- select tps
  selectedCPs <- select cps
  selectedMs <- select ms
  offspringTPs <- (++) selectedTPs <$> offspring priorityCrossover selectedTPs
  offspringCPs <- (++) selectedCPs <$> offspring priorityCrossover selectedCPs
  offspringMs <- (++) selectedMs <$> offspring mappingCrossover selectedMs
  newGenTPs <- mapM (mutate mtPb swapMutate) offspringTPs
  newGenCPs <- mapM (mutate mtPb swapMutate) offspringCPs
  newGenMs <- mapM (mutate mtPb (flipMutate (1, length cs))) offspringMs
  mFits <- mapM (evalMappingFitness fnf tpReps cpReps) newGenMs
  tpFits <- mapM (evalTaskPriorityFitness fnf cpReps mReps) newGenTPs
  cpFits <- mapM (evalCommPriorityFitness fnf tpReps mReps) newGenCPs
  let curBest@(curTPm, curCPm, curM, curFit) = head . sortBy (comparing fourth) $ mFits ++ tpFits ++ cpFits
  let hof' = if curFit < hofFit then (curTPm, curCPm, curM, curFit) else hof
  return $ (map (\(x, _, _, f) -> (x, f)) tpFits
           , map (\(_, y, _, f) -> (y, f)) cpFits
           , map (\(_, _, z, f) -> (z, f)) mFits
           , hof')
    where
      (CCEvolutionParameters _ popSize tournSize cxPb mtPb _) = ep
      (Domain cs _ _) = dom
      select xs = replicateM (ceiling (fromIntegral popSize * (1.0 - cxPb))) (tournament tournSize xs)
      offspring cxF subpop = replicateM (popSize - (length subpop)) (reproduce cxF subpop)

mutate :: (MonadRandom m)
       => Float
       -> ([Int] -> m [Int])
       -> [(Int, Int)]
       -> m [(Int, Int)]
mutate mtPb mutation ind = do
  r <- getRandomR (0.0, 1.0)
  if r > mtPb
    then return ind
    else do
      mutated <- mutation values
      return $ zip subject mutated
        where
          subject = map fst ind
          values = map snd ind

evalTaskPriorityFitness :: (MonadRandom m)
                        => FitnessF
                        -> [CPMap]
                        -> [TMap]
                        -> TPMap
                        -> m (TPMap, CPMap, TMap, Fitness)
evalTaskPriorityFitness fnf cpReps mReps tpMap = do
  scp <- shuffleM cpReps
  sm <- shuffleM mReps
  let reps = zip scp sm
  let rs = map (\(cp, m) -> (tpMap, cp, m, fnf tpMap cp m)) reps
  return . head . sortBy (comparing fourth) $ rs

evalCommPriorityFitness :: (MonadRandom m)
                        => FitnessF
                        -> [TPMap]
                        -> [TMap]
                        -> CPMap
                        -> m (TPMap, CPMap, TMap, Fitness)
evalCommPriorityFitness fnf tpReps mReps cpMap = do
  stp <- shuffleM tpReps
  sm <- shuffleM mReps
  let reps = zip stp sm
  let rs = map (\(tp, m) -> (tp, cpMap, m, fnf tp cpMap m)) reps
  return . head . sortBy (comparing fourth) $ rs

evalMappingFitness :: (MonadRandom m)
                   => FitnessF
                   -> [TPMap]
                   -> [CPMap]
                   -> TMap
                   -> m (TPMap, CPMap, TMap, Fitness)
evalMappingFitness fnf tpReps cpReps mMap = do
  stp <- shuffleM tpReps
  scp <- shuffleM cpReps
  let reps = zip stp scp
  let rs = map (\(tp, cp) -> (tp, cp, mMap, fnf tp cp mMap)) reps
  return . head . sortBy (comparing fourth) $ rs

