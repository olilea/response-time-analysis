
module Main
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

type Fitness = Float

-- Maps task ID to priority
type PMap = [(Int, Int)]

-- Maps task ID to core ID
type TMap = [(Int, Int)]

data Domain = Domain [Core] [Task] Platform

data EvolutionParameters = EvolutionParameters {
    eGenerations :: Int,
    ePopulationSize :: Int,
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

-- Mutation

swapMutate :: (MonadRandom m, Eq a)
           => Float
           -> [a]
           -> m [a]
swapMutate mtPb mp = do
  rs <- take 2 <$> getRandomRs (0, (length mp) - 1)
  let mapping1 = mp !! (rs !! 0)
  let mapping2 = mp !! (rs !! 1)
  let swap x
        | x == mapping1 = mapping2
        | x == mapping2 = mapping1
        | otherwise = x
  return $ map swap mp

-- Crossover

singlePointCrossover :: (MonadRandom m)
                     => [a]
                     -> [a]
                     -> m [a]
singlePointCrossover l r = do
    cxPoint <- getRandomR (0, length l)
    return $ (take cxPoint l) ++ (drop cxPoint r)

mappingCrossover :: (MonadRandom m)
                 => TMap
                 -> TMap
                 -> m TMap
mappingCrossover = singlePointCrossover

-- It must be the case that if two tasks share a priority then
-- there is a priority that has not been assigned to.
-- In this case, randomly pick one of the tasks to retain the given
-- priority.
-- For the other task, assign it a one lower priority
-- and increment the priority of every task whose priority is
-- below this point.
priorityCrossover :: (MonadRandom m)
                  => PMap
                  -> PMap
                  -> m PMap
priorityCrossover l r = undefined

-- Generating the initial population

genTaskMapping :: (MonadRandom m) => Int -> Int -> m [(Int, Int)]
genTaskMapping ts cs = mapM prod [1..ts]
    where
        prod i = do
            r <- getRandomR (1, cs)
            return (i, r)

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
    . reverse
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
    . reverse
    . sortBy (comparing snd)
    . map (\(p, m) -> (p, fnf d p m))
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
        runGA' ep d fnf initialPop 0
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
  (ps, ms) <- genPopulation popSize (length cs) (length ts)
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
          -> (Domain -> PMap -> TMap -> Float)
          -> ([(PMap, Fitness)], [(TMap, Fitness)])
          -> Int
          -> m (PMap, TMap)
runGA' ep@(EvolutionParameters gens _ cxPb mtPb) d fnf pop@(ps, ms) genNumber
  | genNumber == gens = return ((fst . head) ps, (fst . head) ms)
  | otherwise = do
      pop' <- evolve (cxPb, mtPb) d fnf representatives pop
      runGA' ep d fnf pop (genNumber + 1)
        where
          representatives = represent pop

-- Evolve a population into the next generation using the provided
-- fitness function and parameters
evolve :: (MonadRandom m)
       => (Float, Float)
       -> Domain
       -> (Domain -> PMap -> TMap -> Float)
       -> (PMap, TMap)
       -> ([(PMap, Fitness)], [(TMap, Fitness)])
       -> m ([(PMap, Fitness)], [(TMap, Fitness)])
evolve pbs d fnf repr pop@(ps, ms) = undefined

vary :: (MonadRandom m)
     => (Float, Float)
     -> ([PMap], [TMap])
     -> m ([PMap], [TMap])
vary = undefined

main :: IO ()
main = putStrLn $ show $ runGA g ep (Domain cs ts p) (\_ _ _ -> 1.0)
    where
        g = mkStdGen 43
        ep = EvolutionParameters 10 10 0.5 0.5
        cs = [Core idee 1.0 | idee <- [1..9]]
        ts = [Task idee 20.0 20.0 1.0 (Communication (destination idee) 5)
            | idee <- [1..6]]
            where
                destination tIdee = fromJust $ M.lookup tIdee destLookup
                destLookup = M.fromList
                    [ (1, 4)
                    , (2, 4)
                    , (3, 1)
                    , (4, 3)
                    , (5, 2)
                    , (6, 2)
                    ]
        p = Platform 1.0 1.0 1.0
