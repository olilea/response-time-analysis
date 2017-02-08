
module Coevolve
where

import Analysis

import qualified Data.Map as M

import System.Random

-- Need a way to assign fitness to each thing


class (Eq a, Show a) => Genome a where
    fitness :: [a] -> a -> Float
    crossover :: (RandomGen g) => g -> a -> a -> ([a], g)
    mutation :: (RandomGen g) => g -> a -> (a, g)


data EvolutionParameters = EvolutionParameters {
    eGenerations :: Int,
    ePopulationSize :: Int,
    eCrossoverRate :: Float,
    eMutationRate :: Float
}

data Priorities = Priorities PriorityMapping
instance Genome Priorities where
    fitness :: (Mapping m) => a -> m -> Float
    fitness _ _ = 1.0
    crossover g l r = ([l, r], g)
    mutation g a = (a, g)

data Mapping = Mapping TaskMapping
instance Genome Mapping where
    fitness :: (Priorities p) => a -> p -> Float
    fitness = 0.0
    crossover g l r = ([l, r], g)
    mutation g a = (a, g)


coevolve :: EvolutionParameters
         -> Application
         -> Platform
         -> Mapping
coevolve ep a p = Mapping M.empty