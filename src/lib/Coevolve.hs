
module Coevolve
  ( runGA
  , EvolutionParameters(..)
  , Domain(..))
where

import Analysis

import Control.Monad
import Control.Monad.Random

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M

import System.Random
import System.Random.Shuffle

import Debug.Trace


type Fitness = Float

-- Maps task ID to priority
type PMap = [(Int, Int)]

-- Maps task ID to core ID
type TMap = [(Int, Int)]

data Domain = Domain [Core] [Task] Platform

data EvolutionParameters = EvolutionParameters {
    eGenerations :: Int,
    ePopulationSize :: Int,
    eTournamentSize :: Int,
    eCrossoverRate :: Float,
    eMutationRate :: Float
}

pick :: (MonadRandom m) => [a] -> m a
pick as = (!!) as <$> getRandomR (0, (length as) - 1)

-- Choosing representatives

-- Take the best individual as the representative in the next population
represent :: ([(PMap, Fitness)], [(TMap, Fitness)])
          -> (PMap, TMap)
represent (ps, ts) = (extr ps, extr ts)
  where
    extr = fst . head

-- Selection

-- Randomly select individuals from the given population
tournament :: (MonadRandom m)
           => Int
           -> [(a, Float)]
           -> m a
tournament size xs = do
  competitors <- replicateM size (pick xs)
  return . fst . head . sortBy (comparing snd) $ competitors

-- Mutation

-- Should pass in only the assignments
swapMutate :: (MonadRandom m, Eq a)
           => [a]
           -> m [a]
swapMutate as = do
  rs <- take 2 <$> getRandomRs (0, (length as) - 1)
  let mapping1 = as !! (rs !! 0)
  let mapping2 = as !! (rs !! 1)
  let swap x
        | x == mapping1 = mapping2
        | x == mapping2 = mapping1
        | otherwise = x
  return $ map swap as

flipMutate :: (MonadRandom m)
           => (Int, Int)
           -> [Int]
           -> m [Int]
flipMutate range@(low, high) as = do
  chosen <- pick as
  newVal <- pick [low..high]
  let flipper = \x -> case () of
        _ | x == chosen -> newVal
          | otherwise -> x
  return $ map flipper as

-- Crossover

reproduce :: (MonadRandom m)
           => ([(a, a)] -> [(a, a)] -> m [(a, a)])
           -> [[(a, a)]]
           -> m [(a, a)]
reproduce crossoverF population = do
  l <- pick population
  r <- pick population
  crossoverF l r

singlePointCrossover :: (MonadRandom m)
                     => [a]
                     -> [a]
                     -> m [a]
singlePointCrossover l r = do
    cxPoint <- getRandomR (1, (length l) - 1)
    return $ (take cxPoint l) ++ (drop cxPoint r)

mappingCrossover :: (MonadRandom m)
                 => TMap
                 -> TMap
                 -> m TMap
mappingCrossover l r = do
  let sortL = sortBy (comparing fst) l
  let sortR = sortBy (comparing fst) r
  crossedCs <- singlePointCrossover (map snd l) (map snd r)
  return $ zip (map fst sortL) crossedCs

-- It must be the case that if two tasks share a priority then
-- there is a priority that has not been assigned to.
-- In this case, randomly pick one of the tasks to retain the given
-- priority.
-- For the other task, assign it a one lower priority
-- and increment the priority of every task whose priority is
-- below this point.
--
-- TODO: Need to add the random selection between the two
priorityCrossover :: (MonadRandom m)
                  => PMap
                  -> PMap
                  -> m PMap
priorityCrossover l r = do
  let sortL = sortBy (comparing fst) l
  let sortR = sortBy (comparing fst) r
  crossedPs <- singlePointCrossover (map snd l) (map snd r)
  combined <- mapM id $ fixPriorities $ sortBy (comparing snd) . zip (map fst sortL) $ crossedPs
  return $ normalize combined
    where
      normalize ps = map (\(t, p) -> (t, p - highestPriority + 1)) sps
        where
          sps = sortBy (comparing snd) ps
          highestPriority = snd . head $ sps

fixPriorities :: (MonadRandom m) => [(Int, Int)] -> [m (Int, Int)]
fixPriorities ps = case ps of
  (a:[]) -> [return a]
  (a@(at, ap):b@(bt, bp):rem) -> if ap /= bp
    then let diff = abs (ap - bp) in
      if diff > 1
        then (return a):(fixPriorities ((bt, bp - diff + 1):rem))
        else (return a):(fixPriorities (b:rem))
    else do
      (return a):(fixPriorities ((bt, bp + 1):rem))

-- Generating the initial population

genTaskMapping :: (MonadRandom m) => Int -> Int -> m [(Int, Int)]
genTaskMapping ts cs = do
  mappedCores <- replicateM ts (getRandomR (1, cs))
  return $ zip [1..ts] mappedCores

genPriorityMapping :: (MonadRandom m) => Int -> m [(Int, Int)]
genPriorityMapping ts = do
  priorities <- shuffleM [1..ts]
  let tasks = [1..ts]
  return $ zip tasks priorities

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

-- Match a priority mapping to a random task mapping
-- in order to assign a fitness
initialPriorityFitness :: (MonadRandom m)
                       => Domain
                       -> (Domain -> PMap -> TMap -> Float)
                       -> ([PMap], [TMap])
                       -> m [(PMap, Float)]
initialPriorityFitness d@(Domain cs ts _) fnf (ps, ms) = do
  mPicks <- replicateM (length ms) (pick ms)
  -- Take a [PMap] and produce a [(Priorities, Fitness)], sorted
  return
    . sortBy (comparing snd)
    . map (\(p, m) -> (p, fnf d p m))
    $ zip ps mPicks

-- Match a task mapping to a random priority mapping
-- in order to assign a fitness
initialTaskMapFitness :: (MonadRandom m)
                      => Domain
                      -> (Domain -> PMap -> TMap -> Float)
                      -> ([PMap], [TMap])
                      -> m [(TMap, Float)]
initialTaskMapFitness d@(Domain cs ts _) fnf (ps, ms) = do
  pPicks <- replicateM (length ps) (pick ps)
  return
    . sortBy (comparing snd)
    . map (\(p, m) -> (m, fnf d p m))
    $ zip pPicks ms

-- Actual GA implementation

runGA :: (RandomGen g)
         => g
         -> EvolutionParameters
         -> Domain
         -> (Domain -> PMap -> TMap -> Float)
         -> (PMap, TMap)
runGA g ep d@(Domain cs ts p) fnf = evalRand run g
  where
    run = do
        initialPop <- zeroGen popSize d fnf
        runGA' ep d (fnf d) initialPop 0
    popSize = ePopulationSize ep


-- Create an initial population and assign them each a fitness.
-- This fitness is the result of a random pairing across the
-- subpopulations.
-- This fitness is required in order to provide the respresentatives
-- for the initial population
zeroGen :: (MonadRandom m)
        => Int
        -> Domain
        -> (Domain -> PMap -> TMap -> Float)
        -> m ([(PMap, Fitness)], [(TMap, Fitness)])
zeroGen popSize d@(Domain cs ts _) fnf = do
  (!ps, ms) <- genPopulation popSize (length cs) (length ts)
  (,) <$> initialPriorityFitness d fnf (ps, ms)
      <*> initialTaskMapFitness d fnf (ps, ms)

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
runGA' :: (MonadRandom m)
          => EvolutionParameters
          -> Domain
          -> (PMap -> TMap -> Float)
          -> ([(PMap, Fitness)], [(TMap, Fitness)])
          -> Int
          -> m (PMap, TMap)
runGA' ep@(EvolutionParameters gens _ _ cxPb mtPb) dom fnf pop@(ps, ts) genNumber
  | genNumber == gens = return (bestPm, (fst . head . sortBy (comparing snd))ts)
  | otherwise = do
      pop' <- traceShow (((snd . head)  ps, (snd . head) ts)) $ evolve ep dom fnf representatives pop
      runGA' ep dom fnf pop' (genNumber + 1)
    where
      representatives = represent pop
      bestPm = fst . head $ ps
      comparedTms = map (\tm -> fnf bestPm (fst tm)) ts

-- Evolve a population into the next generation using the provided
-- fitness function and parameters
evolve :: (MonadRandom m)
       => EvolutionParameters
       -> Domain
       -> (PMap -> TMap -> Float)
       -> (PMap, TMap)
       -> ([(PMap, Fitness)], [(TMap, Fitness)])
       -> m ([(PMap, Fitness)], [(TMap, Fitness)])
evolve ep dom fnf reps pop@(ps, ts) = do
  selectedPs <- select ps
  selectedTs <- select ts
  offspringPs <- (++) selectedPs <$> offspring priorityCrossover selectedPs
  offspringTs <- (++) selectedTs <$> offspring mappingCrossover selectedTs
  newGenPs <- mapM (mutate mtPb swapMutate) offspringPs
  newGenTs <- mapM (mutate mtPb (flipMutate (1, length cs)))  offspringTs
  return $ evaluateFitness fnf reps (newGenPs, newGenTs)
    where
      (EvolutionParameters _ popSize tournSize cxPb mtPb) = ep
      (Domain cs _ _) = dom
      pOrig = map fst ps
      mOrig = map fst ts
      select xs = replicateM (ceiling (fromIntegral popSize * cxPb)) (tournament tournSize xs)
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

evaluateFitness :: (PMap -> TMap -> Float)
                -> (PMap, TMap)
                -> ([PMap], [TMap])
                -> ([(PMap, Float)], [(TMap, Float)])
evaluateFitness fnf (pRep, mRep) (ps, ts) = (pFit, tFit)
  where
    pFit = map (\p -> (p, fnf p mRep)) ps
    tFit = map (\m -> (m, fnf pRep m)) ts

