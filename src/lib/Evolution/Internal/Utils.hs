
module Evolution.Internal.Utils
  ( third
  , fst3
  , snd3
  , fourth
  , pick
  , tournament
  , flipMutate
  , swapMutate
  , reproduce
  , singlePointCrossover
  , uniformCrossover
  , mappingCrossover
  , coreMappingCrossover
  , priorityCrossover
  , genTaskMapping
  , genPriorityMapping
  , genCoreMapping
  ) where

import Analysis

import Evolution.Internal.Structures

import Control.Monad
import Control.Monad.Random

import Data.List
import Data.Ord

import System.Random.Shuffle


third :: (a, b, c) -> c
third (_, _, x) = x

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

pick :: (MonadRandom m) => [a] -> m a
pick as = (!!) as <$> getRandomR (0, (length as) - 1)

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

coreMappingCrossover :: (MonadRandom m)
                     => CMap
                     -> CMap
                     -> m CMap
coreMappingCrossover l r = do
  let nocSize = floor . sqrt $ fromIntegral (length l)
  let ml = map (\(cId, loc) -> (cId, locationToInt nocSize loc)) l
  let mr = map (\(cId, loc) -> (cId, locationToInt nocSize loc)) r
  crossed <- priorityCrossover ml mr
  return $ map (\(cId, lId) -> (cId, intToLocation nocSize lId)) crossed


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
                  => [(Int, Int)]
                  -> [(Int, Int)]
                  -> m [(Int, Int)]
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

locationToInt :: Int -> Location -> Int
locationToInt nocSize (Location x y) = (nocSize * (pred x)) + y

intToLocation :: Int -> Int -> Location
intToLocation nocSize i = (Location x y)
  where
    x = ceiling ((fromIntegral i) / (fromIntegral nocSize))
    modded = i `mod` nocSize
    y = if modded == 0 then nocSize else modded

-- Initial population generation

genTaskMapping :: (MonadRandom m) => Int -> Int -> m TMap
genTaskMapping ts cs = do
  mappedCores <- replicateM ts (getRandomR (1, cs))
  return $ zip [1..ts] mappedCores

genPriorityMapping :: (MonadRandom m) => Int -> m PMap
genPriorityMapping ts = do
  priorities <- shuffleM [1..ts]
  let tasks = [1..ts]
  return $ zip tasks priorities


-- Assumes the NoC is square
genCoreMapping :: (MonadRandom m) => Int -> m CMap
genCoreMapping cs = do
  locs <- shuffleM [Location r c | r <- [1..nocSize], c <- [1..nocSize]]
  return $ zip [1..cs] locs
  where
    nocSize = floor . sqrt $ fromIntegral cs
