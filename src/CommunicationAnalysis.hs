
module CommunicationAnalysis () where

import Structures

import Control.Lens

type Speed = Float

type CoreId = Id
type CoreSpeed = Float

data Core = Core {
    cId :: CoreId,
    cSpeed :: Float
}

type Network = [[Core]]

-- Zero indexed from the top-left
type Location = (Int, Int)

coreAt :: Location -> Network -> Maybe Core
coreAt (r, c) n = case n ^? element r of
    Just row -> row ^? element c
    Nothing -> Nothing

routeXY :: Location -> Location -> Network -> [Location]
routeXY (ar, ac) (br, bc) n
    | (ar, ac) == (br, bc) = []
    | ac /= bc = case compare ac bc of
        LT -> (ar, ac) : nextCol succ
        GT -> (ar, ac) : nextCol pred
    | ar /= br = case compare ar br of
        LT -> (ar, ac) : nextRow succ
        GT -> (ar, ac) : nextRow pred
    where
        nextCol dir = routeXY (ar, dir ac) (br, bc) n
        nextRow dir = routeXY (dir ar, ac) (br, bc) n
