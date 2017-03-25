
module Evolution.Evolve
  ( runGA
  ) where

import Evolution.Internal.Structures
import Evolution.Internal.Utils

import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies

import Data.List
import Data.Ord

import Debug.Trace

type Ind = (PMap, TMap)
type FInd = (Ind, Fitness)
type HallOfFame = FInd

type Generation = Int

type FitnessF = (PMap -> TMap -> Float)

extractValues :: FInd -> Ind
extractValues ((a, b), f) = (a, b)

genInd :: (MonadRandom m) => Domain -> m Ind
genInd (Domain cs ts _) = do
  tm <- genTaskMapping (length ts) (length cs)
  pm <- genPriorityMapping (length ts)
  return (pm, tm)

zeroGen :: (MonadRandom m)
        => Int
        -> Domain
        -> FitnessF
        -> m [FInd]
zeroGen popSize d fnf = do
  inds <- traceShow 3 $ replicateM popSize (genInd d)
  return $ map (\(pm, tm) -> ((pm, tm), fnf pm tm)) inds

-- Actual GA implementation

runGA :: (RandomGen g)
      => g
      -> EvolutionParameters
      -> Domain
      -> FitnessF
      -> (PMap -> TMap -> Float)
      -> (Ind, [Stat])
runGA g ep d fnf schedf = evalRand run g
  where
    run = do
      initialPop <- zeroGen popSize d fnf
      let initialHof@((ps, ts), fitness) = best initialPop
      let stats = [(Stat 0 fitness (schedf ps ts))]
      (((ps, ts), fit), stats') <- runGA' ep d fnf schedf initialHof initialPop stats 1
      return ((ps, ts), stats')
    popSize = ePopulationSize ep
    best = head . sortBy (comparing snd)

runGA' :: (MonadRandom m)
       => EvolutionParameters
       -> Domain
       -> FitnessF
       -> (PMap -> TMap -> Float)
       -> HallOfFame
       -> [FInd]
       -> [Stat]
       -> Generation
       -> m (HallOfFame, [Stat])
runGA' ep@(EvolutionParameters gens _ _ cxPb mtPb) dom fnf schedf hof pop stats gen
  | gen > gens = return (hof, stats)
  | otherwise = do
      (pop', hof'@((bestPs, bestTs), fitness)) <- evolve ep dom fnf hof pop
      let nextPop = sortBy (comparing snd) pop'
      let stats' = (Stat gen fitness (schedf bestPs bestTs)) : stats
      traceShow (snd hof') $ runGA' ep dom fnf schedf hof' nextPop stats' (succ gen)

evolve :: (MonadRandom m)
       => EvolutionParameters
       -> Domain
       -> FitnessF
       -> HallOfFame
       -> [FInd]
       -> m ([FInd], HallOfFame)
evolve ep dom fnf hof@(_, hofFit) pop = do
  selected <- select pop
  offspring <- (++) selected
    <$> replicateM (popSize - (length selected)) (crossover (map extractValues pop))
  mutated <- mapM (mutate dom mtPb) offspring
  let pop' = map (\ind@(pm, tm) -> (ind, fnf pm tm)) mutated
  let curBest@(_, curFit) = head . sortBy (comparing snd) $ pop'
  let hof' = if curFit < hofFit then curBest else hof
  return (pop', hof')
    where
      (EvolutionParameters _ popSize tournSize cxPb mtPb) = ep
      select :: (MonadRandom m) => [FInd] -> m [Ind]
      select p = replicateM (ceiling (fromIntegral popSize * (1.0 - cxPb))) (tournament tournSize p)

crossover :: (MonadRandom m)
          => [Ind]
          -> m Ind
crossover pop = do
  (lpm, ltm) <- pick pop
  (rpm, rtm) <- pick pop
  opm <- priorityCrossover lpm rpm
  otm <- mappingCrossover ltm rtm
  return (opm, otm)

mutate :: (MonadRandom m)
        => Domain
        -> Float
        -> Ind
        -> m Ind
mutate dom@(Domain cs _ _) mtPb ind@(pm, tm) = do
  r <- getRandomR (0.0, 1.0)
  if r > mtPb
    then return ind
    else do
      mPm <- zip (map fst pm) <$> (swapMutate (map snd pm))
      mTm <- zip (map fst tm) <$> (flipMutate (1, length cs) (map snd tm))
      return (mPm, mTm)
