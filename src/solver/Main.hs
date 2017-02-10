
module Main where

import qualified Data.Map as M
import Data.Maybe

import System.Random

import Analysis
import Coevolve

-- TODO: Add functions to generate task sets
-- TODO: Implement breakpoint frequency function

main :: IO ()
main = do
  let mappings = runGA g ep d fnf
  putStrLn . show $ mappings
  putStrLn . show $ fnf d (fst mappings) (snd mappings)
  putStrLn . show $ end d (fst mappings) (snd mappings)
    where
        g = mkStdGen 44535432443
        ep = EvolutionParameters 200 100 5 60 0.5 0.05
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
        coreMapping = M.fromList $ zip [1..9] [Location r c | r <- [1..3], c <- [1..3]]
        p = Platform 1.0 1.0 1.0
        d = Domain cs ts p
        fnf d pm tm = fromIntegral $ missingDeadlines p 3.0 (Application cs ts (M.fromList tm) coreMapping (M.fromList pm))

        end d pm tm = endToEnd p 3.0 (Application cs ts (M.fromList tm) coreMapping (M.fromList pm))
