{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Algorithms (aStar, Distance (..), distify) where

import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as M
import qualified Data.HashMap.Strict as SM
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as PSQ
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Control.DeepSeq (deepseq, NFData (rnf))
import Control.Parallel.Strategies (NFData)

-- import Debug.Trace (trace)

data Distance = Distance Int | Infinity deriving (Eq, Show)

(.+.) :: Distance -> Distance -> Distance
Infinity .+. _ = Infinity
_ .+. Infinity = Infinity
(Distance a) .+. (Distance b) = Distance (a + b)

fromDist :: Distance -> Int
fromDist (Distance i) = i
fromDist Infinity = undefined

instance Ord Distance where
    (<=) :: Distance -> Distance -> Bool
    Infinity <= Infinity = True
    Infinity <= _ = False
    _ <= Infinity = True
    Distance a <= Distance b = a <= b

instance NFData Distance where
  rnf Infinity = ()
  rnf (Distance q) = q `seq` ()

type GMap a = SM.HashMap a Int
type OpenSet a = HashPSQ a Int ()

lookUp :: (Hashable k, Ord k) => GMap k -> k -> Distance
lookUp m i = maybe Infinity Distance (SM.lookup i m)

-- | 'aStar': does stuff stuff: 
--
-- > startnode goalpred h neighbors
--
-- /Since: 1.1.0.0/
aStar :: (Hashable a, Ord a, Show a, NFData a) => [a] -> (a -> Bool) -> (a -> Int) -> (a -> [(a, Int)]) -> Distance
aStar startnodes goalpred h nf = aStar' openSet gScores goalpred h nf
  where
    openSet = PSQ.fromList $ map (\sn -> (sn, h sn, ())) startnodes
    gScores = SM.fromList $ map (\sn -> (sn, 0)) startnodes

aStar' ::
    (Hashable a, Ord a, Show a, NFData a) =>
    OpenSet a -> -- openSet
    GMap a -> -- gScores
    (a -> Bool) -> -- goalnode
    (a -> Int) -> -- h
    (a -> [(a, Int)]) -> -- nf
    Distance -- distance
aStar' openSet gScores goalpred h neighbors
    | PSQ.null openSet = Infinity
    | goalpred current = Distance currentG
    | otherwise = aStar' updatedOpenSet gScores'' goalpred h neighbors
  where
    (current, _, _, poppedOpenSet) = fromMaybe undefined (PSQ.minView openSet)
    currentG = gScores SM.!  current

    (updatedOpenSet, gScores'') =  let n = neighbors current in foldl' go (poppedOpenSet, gScores) n

    go (os, gs) (nn, d)
        | Distance tentdist >= lookUp gs nn = (os, gs)
        | otherwise =
            ( PSQ.insert nn (tentdist  + h nn) () os
            , SM.insert nn tentdist gs
            )
      where
        tentdist = currentG +  d

--
distify :: [(a, Int)] -> [(a, Distance)]
distify l = second Distance <$> l

-- neighborsFromMapWithDist :: (Hashable a, Ord a) => Map a [(a, Int)] -> a -> [(a, Distance)]
-- neighborsFromMapWithDist m a = distify (m M.! a)

-- neighborsFromMap :: (Hashable a, Ord a) => Map a [a] -> a -> [(a, Distance)]
-- neighborsFromMap m a = (,Distance 1) <$> (m M.! a)