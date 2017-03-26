
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

type HallOfFame = (PMap, CMap, TMap, Fitness)


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
               -> m ([PMap], [TMap])
genPopulation popSize cs ts = do
    let populate = replicateM popSize
    ps <- populate $ genPriorityMapping ts
    ms <- populate $ genTaskMapping ts cs
    return (ps, ms)

-- Initial fitness calculation

-- Match a priority mapping to a random task mapping
-- in order to assign a fitness
initialPriorityFitness :: (MonadRandom m)
                       => (PMap -> TMap -> Fitness)
                       -> ([PMap], [TMap])
                       -> m [(PMap, Fitness)]
initialPriorityFitness fnf (ps, ms) = do
  mPicks <- replicateM (length ms) (pick ms)
  -- Take a [PMap] and produce a [(Priorities, Fitness)], sorted
  return
    . sortBy (comparing snd)
    . map (\(p, m) -> (p, fnf p m))
    $ zip ps mPicks

-- Match a task mapping to a random priority mapping
-- in order to assign a fitness
initialTaskMapFitness :: (MonadRandom m)
                      => (PMap -> TMap -> Fitness)
                      -> ([PMap], [TMap])
                      -> m [(TMap, Float)]
initialTaskMapFitness fnf (ps, ms) = do
  pPicks <- replicateM (length ps) (pick ps)
  return
    . sortBy (comparing snd)
    . map (\(p, m) -> (m, fnf p m))
    $ zip pPicks ms

-- Actual GA implementation

runCCGA :: (RandomGen g)
         => g
         -> CCEvolutionParameters
         -> Domain
         -> (PMap -> TMap -> Fitness)
         -> (PMap -> TMap -> Float)
         -> ((PMap, TMap), [Stat])
runCCGA g ep d@(Domain cs ts p) fnf schedf = evalRand run g
  where
    run = do
        initialPop <- zeroGen popSize d fnf
        let bestPm = getBest . fst $ initialPop
        let bestTm = getBest . snd $ initialPop
        let initialHof@(bestPs, bestTs, fitness) = (bestPm, bestTm, fnf bestPm bestTm)
        let stats = [(Stat 0 fitness (schedf bestPs bestTs))]
        (finalHof@(pm, tm, _), stats) <- runCCGA' ep d fnf schedf initialHof initialPop stats 1
        return ((pm, tm), stats)
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
        -> (PMap -> TMap -> Float)
        -> m ([(PMap, Fitness)], [(TMap, Fitness)])
zeroGen popSize d@(Domain cs ts _) fnf = do
  (!ps, ms) <- genPopulation popSize (length cs) (length ts)
  (,) <$> initialPriorityFitness fnf (ps, ms)
      <*> initialTaskMapFitness fnf (ps, ms)

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
          -> (PMap -> TMap -> Fitness)
          -> (PMap -> TMap -> Float)
          -> HallOfFame
          -> ([(PMap, Fitness)], [(TMap, Fitness)])
          -> [Stat]
          -> Int
          -> m (HallOfFame, [Stat])
runCCGA' ep@(CCEvolutionParameters gens _ _ cxPb mtPb poolSize) dom fnf schedf hof pop@(ps, ts) stats genNumber
  | genNumber > gens = traceShow ts $ return (hof, stats)
  | otherwise = do
      pReps <- represent poolSize ps
      tReps <- represent poolSize ts
      (ps', ts', hof'@(bestPs, bestTs, bestFit)) <- evolve ep dom fnf hof (pReps, tReps) pop
      let pop' = (sortBy (comparing snd) ps', sortBy (comparing snd) ts')
      let stats' = (Stat genNumber bestFit (schedf bestPs bestTs)) : stats
      traceShow bestFit $ runCCGA' ep dom fnf schedf hof' pop' stats' (genNumber + 1)
    where
      bestPm = fst . head $ sortBy (comparing snd) ps
      comparedTms = map (\(tm, _) -> (tm, fnf bestPm tm)) ts

-- Evolve a population into the next generation using the provided
-- fitness function and parameters
evolve :: (MonadRandom m)
       => CCEvolutionParameters
       -> Domain
       -> (PMap -> TMap -> Fitness)
       -> HallOfFame
       -> ([PMap], [TMap])
       -> ([(PMap, Fitness)], [(TMap, Fitness)])
       -> m ([(PMap, Fitness)], [(TMap, Fitness)], HallOfFame)
evolve ep dom fnf hof@(hofPm, hofTm, hofFit) reps@(pReps, tReps) pop@(ps, ts) = do
  selectedPs <- select ps
  selectedTs <- select ts
  offspringPs <- (++) selectedPs <$> offspring priorityCrossover selectedPs
  offspringTs <- (++) selectedTs <$> offspring mappingCrossover selectedTs
  newGenPs <- mapM (mutate mtPb swapMutate) offspringPs
  newGenTs <- mapM (mutate mtPb (flipMutate (1, length cs))) offspringTs
  let tFits = map (evalMappingFitness fnf pReps) newGenTs `using` parList rdeepseq
  let pFits = map (evalPriorityFitness fnf tReps) newGenPs `using` parList rdeepseq
  let curBest@(curPm, curTm, curFit) = head . sortBy (comparing third) $ tFits ++ pFits
  let hof' = if curFit < hofFit then (curPm, curTm, curFit) else hof
  return $ (map (\(x, _, f) -> (x, f)) pFits
           , map (\(_, y, f) -> (y, f)) tFits
           , hof')
    where
      (CCEvolutionParameters _ popSize tournSize cxPb mtPb _) = ep
      (Domain cs _ _) = dom
      pOrig = map fst ps
      mOrig = map fst ts
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

evalPriorityFitness :: (PMap -> TMap -> Fitness)
                    -> [TMap]
                    -> PMap
                    -> (PMap, TMap, Fitness)
evalPriorityFitness fnf reps pmap = head . sortBy (comparing third) $ rs
  where
    rs = map (\r -> (pmap, r, fnf pmap r)) reps

evalMappingFitness :: (PMap -> TMap -> Fitness)
                   -> [PMap]
                   -> TMap
                   -> (PMap, TMap, Fitness)
evalMappingFitness fnf reps tmap = head . sortBy (comparing third) $ rs
  where
    rs = map (\r -> (r, tmap, fnf r tmap)) reps

