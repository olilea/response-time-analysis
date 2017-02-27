
module Coevolve
  ( runGA
  , EvolutionParameters(..)
  , Domain(..))
where

import Analysis

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

type Fitness = Float

-- Maps task ID to priority
type PMap = [(TaskId, TaskPriority)]

-- Maps task ID to core ID
type TMap = [(TaskId, CoreId)]

type HallOfFame = (PMap, TMap, Fitness)

data Domain = Domain [Core] [Task] Platform

data EvolutionParameters = EvolutionParameters {
    eGenerations :: Int,
    ePopulationSize :: Int,
    eTournamentSize :: Int,
    eCrossoverRate :: Float,
    eMutationRate :: Float,
    eRepPoolSize :: Int
}

pick :: (MonadRandom m) => [a] -> m a
pick as = (!!) as <$> getRandomR (0, (length as) - 1)

third :: (a, b, c) -> c
third (_, _, x) = x

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

-- Selection

-- Randomly select individuals from the given population to take
-- part in a tournament. If more than one have the same fitness, return
-- a random one.
tournament :: (MonadRandom m)
           => Int
           -> [(a, Float)]
           -> m a
tournament size xs = do
  competitors <- replicateM size (pick xs)
  let s = sortBy (comparing snd) competitors
  let top = takeWhile (\(a, fitness) -> fitness == (snd . head) s) s
  fst <$> pick top

-- Mutation

-- Should pass in only the assignments
swapMutate :: (MonadRandom m, Eq a)
           => [a]
           -> m [a]
swapMutate as = do
  rs <- take 2 . nub <$> getRandomRs (0, (length as) - 1)
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
  newVal <- pick [i | i <- [low..high], i /= chosen]
  let flipper = \x -> case () of
        _ | x == chosen -> newVal
          | otherwise -> x
  return $ map flipper as

-- Crossover

reproduce :: (MonadRandom m)
           => (a -> a -> m a)
           -> [a]
           -> m a
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

uniformCrossover :: (MonadRandom m)
                 => [a]
                 -> [a]
                 -> m [a]
uniformCrossover ls rs = sequence . map (\(l, r) -> pick [l, r]) $ zip ls rs

mappingCrossover :: (MonadRandom m)
                 => TMap
                 -> TMap
                 -> m TMap
mappingCrossover l r = do
  let sortL = sortBy (comparing fst) l
  let sortR = sortBy (comparing fst) r
  crossedCs <- uniformCrossover (map snd sortL) (map snd sortR)
  return . zip (map fst sortL) $ crossedCs

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
  crossedPs <- uniformCrossover (map snd sortL) (map snd sortR)
  combined <- sequence . fixPriorities . sortBy (comparing snd) . zip (map fst sortL) $ crossedPs
  return . normalize $ combined
    where
      normalize ps = sortBy (comparing fst) normalized
        where
          sps = sortBy (comparing snd) ps
          highestPriority = snd . head $ sps
          normalized = map (\(t, p) -> (t, p - highestPriority + 1)) sps

fixPriorities :: (MonadRandom m) => [(Int, Int)] -> [m (Int, Int)]
fixPriorities ps = case ps of
  (a:[]) -> [return a]
  (a@(at, ap):b@(bt, bp):rem) -> if ap /= bp
    then let diff = abs (ap - bp) in
      if bp < ap then (return a):(fixPriorities ((bt, bp + 2):rem)) else
      if diff > 1
        then (return a):(fixPriorities ((bt, bp - diff + 1):rem))
        else (return a):(fixPriorities (b:rem))
    else do
      (return b):(fixPriorities ((at, ap + 1):rem))

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

-- Match a priority mapping to a random task mapping
-- in order to assign a fitness
initialPriorityFitness :: (MonadRandom m)
                       => Domain
                       -> (Domain -> PMap -> TMap -> Float)
                       -> ([PMap], [TMap])
                       -> m [(PMap, Fitness)]
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
        let bestPm = getBest . fst $ initialPop
        let bestTm = getBest . snd $ initialPop
        let initialHof = (bestPm, bestTm, reducedFnf bestPm bestTm)
        finalHof@(pm, tm, f) <- runGA' ep d reducedFnf initialHof initialPop 0
        return (pm, tm)
    popSize = ePopulationSize ep
    getBest = fst . head . sortBy (comparing snd)
    reducedFnf = fnf d


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
          -> (PMap -> TMap -> Fitness)
          -> HallOfFame
          -> ([(PMap, Fitness)], [(TMap, Fitness)])
          -> Int
          -> m HallOfFame
runGA' ep@(EvolutionParameters gens _ _ cxPb mtPb poolSize) dom fnf hof pop@(ps, ts) genNumber
  | genNumber == gens = traceShow ts $ return hof
  | otherwise = do
      pReps <- represent poolSize ps
      tReps <- represent poolSize ts
      (ps', ts', hof') <- evolve ep dom fnf hof (pReps, tReps) pop
      let pop' = (sortBy (comparing snd) ps', sortBy (comparing snd) ts')
      traceShow (third hof') $ runGA' ep dom fnf hof' pop' (genNumber + 1)
    where
      bestPm = fst . head $ sortBy (comparing snd) ps
      comparedTms = map (\(tm, _) -> (tm, fnf bestPm tm)) ts

-- Evolve a population into the next generation using the provided
-- fitness function and parameters
evolve :: (MonadRandom m)
       => EvolutionParameters
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
      (EvolutionParameters _ popSize tournSize cxPb mtPb _) = ep
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

