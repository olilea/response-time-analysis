
module Evolution.Internal.Structures
  ( EvolutionParameters(..)
  , CCEvolutionParameters(..)
  , Domain(..)
  , Fitness
  , PMap
  , TMap)
  where

import Analysis

data EvolutionParameters = EvolutionParameters {
    eGenerations :: Int,
    ePopulationSize :: Int,
    eTournamentSize :: Int,
    eCrossoverRate :: Float,
    eMutationRate :: Float
}

data CCEvolutionParameters = CCEvolutionParameters {
    ceGenerations :: Int,
    cePopulationSize :: Int,
    ceTournamentSize :: Int,
    ceCrossoverRate :: Float,
    ceMutationRate :: Float,
    ceRepPoolSize :: Int
}

data Domain = Domain [Core] [Task] Platform

type Fitness = Float

-- Maps task ID to priority
type PMap = [(TaskId, TaskPriority)]

-- Maps task ID to core ID
type TMap = [(TaskId, CoreId)]
