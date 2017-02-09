
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

type Population = ([(Priorities, Float)], [(Mapping, Float)])

-- Maps task ID to priority
type Priorities = [(Int, Int)]
-- Maps task ID to core ID
type Mapping = [(Int, Int)]

data Domain = Domain [Core] [Task] Platform

data EvolutionParameters = EvolutionParameters {
    eGenerations :: Int,
    ePopulationSize :: Int,
    eCrossoverRate :: Float,
    eMutationRate :: Float
}

pick :: (MonadRandom m) => [a] -> m a
pick as = (!!) as <$> getRandomR (0, (length as) - 1)

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
                 => Mapping
                 -> Mapping
                 -> m Mapping
mappingCrossover = singlePointCrossover

-- It must be the case that if two tasks share a priority then
-- there is a priority that is not being used.
-- In this case, randomly pick one of the tasks to retain the given
-- priority.
-- For the other task, assign it a one lower priority
-- and increment the priority of every task whose priority is
-- below this point.
priorityCrossover :: (MonadRandom m)
                  => Priorities
                  -> Priorities
                  -> m Priorities
priorityCrossover l r = singlePointCrossover (order l) (order r)
  where
    order = sortBy (comparing fst)

-- Generating the initial population

genMapping :: (MonadRandom m) => Int -> Int -> m Mapping
genMapping ts cs = mapM prod [1..ts]
    where
        prod i = do
            r <- getRandomR (1, cs)
            return (i, r)

genPriorities :: (MonadRandom m) => Int -> m Priorities
genPriorities ts = do
  priorities <- shuffleM [1..ts]
  let tasks = [1..ts]
  return $ zip tasks priorities

genPopulations :: (MonadRandom m)
               => Int
               -> Int
               -> Int
               -> m ([Priorities], [Mapping])
genPopulations popSize cs ts = do
    let populate = replicateM popSize
    ps <- populate $ genPriorities ts
    ms <- populate $ genMapping ts cs
    return (ps, ms)

-- Actual GA implementation

runGA :: (RandomGen g)
         => g
         -> EvolutionParameters
         -> Domain
         -> (Domain -> Priorities -> Mapping -> Float)
         -> (Priorities, Mapping)
runGA g ep d@(Domain cs ts p) fnf = evalRand run g
  where
    run = do
        pop <- zeroGen ep d fnf
        runGA' ep d fnf pop 0

initialFitnessPri :: (MonadRandom m)
                 => Domain
                 -> (Domain -> Priorities -> Mapping -> Float)
                 -> ([Priorities], [Mapping])
                 -> m [(Priorities, Float)]
initialFitnessPri d@(Domain cs ts _) fnf (ps, ms) = do
  mPicks <- replicateM (length ms) (pick ms)
  -- Take a [Priorities] and produce a [(Priorities, Fitness)], sorted
  return
    . reverse
    . sortBy (comparing snd)
    . map (\(p, m) -> (p, fnf d p m))
    $ zip ps mPicks

initialFitnessMap :: (MonadRandom m)
                  => Domain
                  -> (Domain -> Priorities -> Mapping -> Float)
                  -> ([Priorities], [Mapping])
                  -> m [(Mapping, Float)]
initialFitnessMap d@(Domain cs ts _) fnf (ps, ms) = do
  pPicks <- replicateM (length ps) (pick ps)
  -- Take a [Mapping] and produce a [(Mapping, Fitness)], sorted.
  return
    . reverse
    . sortBy (comparing snd)
    . map (\(p, m) -> (p, fnf d p m))
    $ zip pPicks ms

-- Create an initial population and assign them each a fitness.
-- This fitness is the result of a random pairing across the
-- subpopulations.
zeroGen :: (MonadRandom m)
        => EvolutionParameters
        -> Domain
        -> (Domain -> Priorities -> Mapping -> Float)
        -> m Population
zeroGen ep@(EvolutionParameters popSize _ _ _) d@(Domain cs ts _) fnf = do
  (ps, ms) <- genPopulations popSize (length cs) (length ts)
  (,) <$> initialFitnessPri d fnf (ps, ms)
      <*> initialFitnessMap d fnf (ps, ms)

-- The recursive part of the GA
runGA' :: (MonadRandom m)
          => EvolutionParameters
          -> Domain
          -> (Domain -> Priorities -> Mapping -> Float)
          -> Population
          -> Int
          -> m (Priorities, Mapping)
runGA' ep@(EvolutionParameters gens _ cxPb mtPb) d fnf pop@(ps, ms) genNumber
  | genNumber == gens = return ((fst . head) ps, (fst . head) ms)
  | otherwise = do
      pop' <- evolve ep d fnf pop
      runGA' ep d fnf pop (genNumber + 1)

-- Evolve a population into the next generation using the provided
-- fitness function and parameters
evolve :: (MonadRandom m)
       => EvolutionParameters
       -> Domain
       -> (Domain -> Priorities -> Mapping -> Float)
       -> Population 
       -> m Population
evolve ep@(EvolutionParameters _ _ cxPb mtPb) d fnf pop@(ps, ms) = return pop

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
