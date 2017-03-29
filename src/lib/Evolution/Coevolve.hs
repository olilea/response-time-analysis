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

type HallOfFame = (PMap, TMap, CMap, Fitness)

type FitnessF = (PMap -> TMap -> CMap -> Fitness)
type Population = ([PMap], [TMap], [CMap])

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
               -> m Population
genPopulation popSize cs ts = do
    let populate = replicateM popSize
    ps <- populate $ genPriorityMapping ts
    ms <- populate $ genTaskMapping ts cs
    cs <- populate $ genCoreMapping cs
    return (ps, ms, cs)

-- Initial fitness calculation

-- Match a priority mapping to a random task mapping
-- in order to assign a fitness
initialPriorityFitness :: (MonadRandom m)
                       => FitnessF
                       -> Population
                       -> m [(PMap, Fitness)]
initialPriorityFitness fnf (ps, ms, cs) = do
  mPicks <- replicateM (length ms) (pick ms)
  cPicks <- replicateM (length cs) (pick cs)
  -- Take a [PMap] and produce a [(Priorities, Fitness)], sorted
  return
    . sortBy (comparing snd)
    . map (\(p, m, c) -> (p, fnf p m c))
    $ zip3 ps mPicks cPicks

-- Match a task mapping to a random priority mapping
-- in order to assign a fitness
initialTaskMapFitness :: (MonadRandom m)
                      => FitnessF
                      -> Population
                      -> m [(TMap, Fitness)]
initialTaskMapFitness fnf (ps, ms, cs) = do
  pPicks <- replicateM (length ps) (pick ps)
  cPicks <- replicateM (length cs) (pick cs)
  return
    . sortBy (comparing snd)
    . map (\(p, m, c) -> (m, fnf p m c))
    $ zip3 pPicks ms cPicks

initialCoreMapFitness :: (MonadRandom m)
                      => FitnessF
                      -> Population
                      -> m [(CMap, Fitness)]
initialCoreMapFitness fnf (ps, ts, cs) = do
  pPicks <- replicateM (length ps) (pick ps)
  tPicks <- replicateM (length ts) (pick ts)
  return
    . sortBy (comparing snd)
    . map (\(p, m, c) -> (c, fnf p m c))
    $ zip3 pPicks tPicks cs

-- Actual GA implementation

runCCGA :: (RandomGen g)
         => g
         -> CCEvolutionParameters
         -> Domain
         -> FitnessF
         -> (PMap -> TMap -> CMap -> Float)
         -> ((PMap, TMap, CMap), [Stat])
runCCGA g ep d@(Domain cs ts p) fnf schedf = evalRand run g
  where
    run = do
        initialPop <- zeroGen popSize d fnf
        let bestPm = getBest . fst3 $ initialPop
        let bestTm = getBest . snd3 $ initialPop
        let bestCm = getBest . third $ initialPop
        let initialHof@(bestPs, bestTs, bestCs, fitness) = (bestPm, bestTm, bestCm, fnf bestPm bestTm bestCm)
        let stats = [(Stat 0 fitness (schedf bestPs bestTs bestCs))]
        (finalHof@(pm, tm, cm, _), stats) <- runCCGA' ep d fnf schedf initialHof initialPop stats 1
        return ((pm, tm, cm), stats)
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
        -> m ([(PMap, Fitness)], [(TMap, Fitness)], [(CMap, Fitness)])
zeroGen popSize d@(Domain cs ts _) fnf = do
  (ps, ms, cs) <- genPopulation popSize (length cs) (length ts)
  (,,) <$> initialPriorityFitness fnf (ps, ms, cs)
       <*> initialTaskMapFitness fnf (ps, ms, cs)
       <*> initialCoreMapFitness fnf (ps, ms, cs)

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
          -> (PMap -> TMap -> CMap -> Float)
          -> HallOfFame
          -> ([(PMap, Fitness)], [(TMap, Fitness)], [(CMap, Fitness)])
          -> [Stat]
          -> Int
          -> m (HallOfFame, [Stat])
runCCGA' ep@(CCEvolutionParameters gens _ _ cxPb mtPb poolSize) dom fnf schedf hof pop@(ps, ts, cs) stats genNumber
  | genNumber > gens = traceShow ts $ return (hof, stats)
  | otherwise = do
      pReps <- represent poolSize ps
      tReps <- represent poolSize ts
      cReps <- represent poolSize cs
      (ps', ts', cs', hof'@(bestPs, bestTs, bestCs, bestFit)) <- evolve ep dom fnf hof (pReps, tReps, cReps) pop
      let pop' = (sortBy (comparing snd) ps', sortBy (comparing snd) ts', sortBy (comparing snd) cs')
      let stats' = (Stat genNumber bestFit (schedf bestPs bestTs bestCs)) : stats
      traceShow bestFit $ runCCGA' ep dom fnf schedf hof' pop' stats' (genNumber + 1)

-- Evolve a population into the next generation using the provided
-- fitness function and parameters
evolve :: (MonadRandom m)
       => CCEvolutionParameters
       -> Domain
       -> FitnessF
       -> HallOfFame
       -> ([PMap], [TMap], [CMap])
       -> ([(PMap, Fitness)], [(TMap, Fitness)], [(CMap, Fitness)])
       -> m ([(PMap, Fitness)], [(TMap, Fitness)], [(CMap, Fitness)], HallOfFame)
evolve ep dom fnf hof@(hofPm, hofTm, hofCm, hofFit) reps@(pReps, tReps, cReps) pop@(ps, ts, cs) = do
  selectedPs <- select ps
  selectedTs <- select ts
  selectedCs <- select cs
  offspringPs <- (++) selectedPs <$> offspring priorityCrossover selectedPs
  offspringTs <- (++) selectedTs <$> offspring mappingCrossover selectedTs
  offspringCs <- (++) selectedCs <$> offspring coreMappingCrossover selectedCs
  newGenPs <- mapM (mutate mtPb swapMutate) offspringPs
  newGenTs <- mapM (mutate mtPb (flipMutate (1, length cores))) offspringTs
  newGenCs <- mapM (mutate mtPb swapMutate) offspringCs
  tFitsM <- (mapM (evalMappingFitness fnf pReps cReps) newGenTs)
  let tFits = tFitsM `using` parList rdeepseq
  pFitsM <- mapM (evalPriorityFitness fnf tReps cReps) newGenPs
  let pFits = pFitsM `using` parList rdeepseq
  cFitsM <- mapM (evalCoreMappingFitness fnf pReps tReps) newGenCs
  let cFits = cFitsM `using` parList rdeepseq
  let curBest@(curPm, curTm, curCm, curFit) = head . sortBy (comparing fourth) $ tFits ++ pFits ++ cFits
  let hof' = if curFit < hofFit then (curPm, curTm, curCm, curFit) else hof
  return $ (map (\(x, _, _, f) -> (x, f)) pFits
           , map (\(_, y, _, f) -> (y, f)) tFits
           , map (\(_, _, z, f) -> (z, f)) cFits
           , hof')
    where
      (CCEvolutionParameters _ popSize tournSize cxPb mtPb _) = ep
      (Domain cores _ _) = dom
      pOrig = map fst ps
      mOrig = map fst ts
      select xs = replicateM (ceiling (fromIntegral popSize * (1.0 - cxPb))) (tournament tournSize xs)
      offspring cxF subpop = replicateM (popSize - (length subpop)) (reproduce cxF subpop)

mutate :: (MonadRandom m)
       => Float
       -> ([a] -> m [a])
       -> [(b, a)]
       -> m [(b, a)]
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

evalPriorityFitness :: (MonadRandom m)
                    => FitnessF
                    -> [TMap]
                    -> [CMap]
                    -> PMap
                    -> m (PMap, TMap, CMap, Fitness)
evalPriorityFitness fnf tReps cReps pmap = do
  st <- shuffleM tReps
  sc <- shuffleM cReps
  let reps = zip st sc
  let rs = map (\(t, c) -> (pmap, t, c, fnf pmap t c)) reps
  return . head . sortBy (comparing fourth) $ rs

evalMappingFitness :: (MonadRandom m)
                   => FitnessF
                   -> [PMap]
                   -> [CMap]
                   -> TMap
                   -> m (PMap, TMap, CMap, Fitness)
evalMappingFitness fnf pReps cReps tmap = do
  sp <- shuffleM pReps
  sc <- shuffleM cReps
  let reps = zip sp sc
  let rs = map (\(p, c) -> (p, tmap, c, fnf p tmap c)) reps
  return . head . sortBy (comparing fourth) $ rs


evalCoreMappingFitness :: (MonadRandom m)
                   => FitnessF
                   -> [PMap]
                   -> [TMap]
                   -> CMap
                   -> m (PMap, TMap, CMap, Fitness)
evalCoreMappingFitness fnf pReps tReps cmap = do
  sp <- shuffleM pReps
  st <- shuffleM tReps
  let reps = zip sp st
  let rs = map (\(p, t) -> (p, t, cmap, fnf p t cmap)) reps
  return . head . sortBy (comparing fourth) $ rs
