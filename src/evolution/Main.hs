
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

-- Need a way to assign fitness to each thing

-- Maps priority to task ID
type Priorities = [(Int, Int)]
-- Maps task ID to core ID
type Mapping = [(Int, Int)]

data EvolutionParameters = EvolutionParameters {
    eGenerations :: Int,
    ePopulationSize :: Int,
    eCrossoverRate :: Float,
    eMutationRate :: Float
}

singlePointCrossover :: (MonadRandom m)
                     => [a]
                     -> [a]
                     -> m [a]
singlePointCrossover l r = do
    cxPoint <- getRandomR (0, length l)
    return $ (take cxPoint l) ++ (drop cxPoint r)

-- mappingcrossover :: (RandomGen g)
--                  => g 
--                  -> TaskMapping
--                  -> TaskMapping
--                  -> Rand g TaskMapping
-- mappingCrossover g tmLeft tmRight = (M.fromList offspring, g') = do
--     let order = sortBy (comparing fst) . M.toList
--     let (sl, sr) = (order tmLeft, order tmRight)

genMapping :: (MonadRandom m) => Int -> Int -> m Mapping
genMapping ts cs = mapM prod [1..ts]
    where
        prod i = do
            r <- getRandomR (1, cs)
            return (i, r)

genPriorities :: (MonadRandom m) => Int -> m Priorities
genPriorities ts = mapM prod priorities
    where
        priorities = [1..ts]
        prod i = do 
            r <- getRandomR (1, ts)
            return (i, r)

genPopulations :: (MonadRandom m)
               => Int 
               -> Int
               -> Int 
               -> m ([Priorities], [Mapping])
genPopulations popSize ts cs = do
    let populate = replicateM popSize
    ps <- populate $ genPriorities ts
    ms <- populate $ genMapping ts cs
    return (ps, ms)

coevolve :: (RandomGen g)
         => g
         -> EvolutionParameters
         -> [Core]
         -> [Task]
         -> Platform
         -> ([Priorities], [Mapping])
coevolve g ep cs ts p = evalRand evolve g
    where
        evolve = genPopulations (ePopulationSize ep) (length ts) (length cs)

cores = [Core idee 1.0 | idee <- [1..9]]


main :: IO ()
main = putStrLn $ show $ coevolve g ep cs ts p
    where
        g = mkStdGen 42
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
