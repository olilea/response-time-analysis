
module Coevolve
where

import Analysis

import qualified Data.Map as M

import System.Random

-- Need a way to assign fitness to each thing

data Individual = Individual PriorityMapping TaskMapping

data Solution = Solution PriorityMapping TaskMapping
data Population = Population [PriorityMapping] [TaskMapping]

data EvolutionParameters = EvolutionParameters {
    eGenerations :: Int,
    ePopulationSize :: Int,
    eCrossoverRate :: Float,
    eMutationRate :: Float
}

randomPriorityMapping :: PriorityMapping
randomPriorityMapping = M.empty

randomTaskMapping :: TaskMapping
randomTaskMapping = M.empty

generationR :: Platform
            -> Application
            -> EvolutionParameters
            -> Population
            -> Int
            -> Solution
generationR _ _ _ sol 0 = Solution M.empty M.empty
generationR p a ep pops genRemain = generationR p a ep nextPop (genRemain-1)
    where
        nextPop = pops

coevolve :: Platform
         -> Application 
         -> EvolutionParameters
         -> Solution
coevolve p a ep = generationR p a ep pops (eGenerations ep)
    where
        popSize = ePopulationSize ep
        pops = Population [randomPriorityMapping | x <- [1..popSize]] [randomTaskMapping | x <- [1..popSize]]
    