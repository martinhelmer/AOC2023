{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}

module Day17 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import AOCHelper (readInpByteSTring)
import Algorithms (aStar, Distance (Distance), neighborsFromMapWithDist, distify)
import BSArray (BSArray, makeBSarray)
import qualified BSArray as BSA
import Data.Char (digitToInt, ord)
import RunUtil (RunMe, runMeByteString)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (second, Bifunctor (first))
import Control.DeepSeq (NFData, rnf, deepseq)

example :: ByteString
example =
  [r|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533|]

runex :: RunMe
runex =
  runMeByteString
    "Day 17 - example"
    (return example)
    part1
    (Just 102)
    part2
    (Just 94)

runme :: RunMe
runme =
  runMeByteString
    "Day 17: Clumsy Crucible "
    (readInpByteSTring "day17.txt")
    part1
    (Just 674)
    part2
    (Just 773)

---
data Orientation = Vertical | Horizontal deriving (Show, Ord, Eq, Enum)

not' :: Orientation -> Orientation
not' Vertical = Horizontal
not' Horizontal = Vertical

directions :: Orientation -> [Dir]
directions o  = case o of
    Vertical -> [NORTH, SOUTH]
    Horizontal -> [EAST, WEST]

newtype Pos = Pos (Int, Int) deriving (Eq, Show, Ord)
data Dir = NORTH | EAST | SOUTH | WEST deriving (Eq, Show, Enum)

(.+.) :: Pos -> Pos -> Pos
(.+.) (Pos (a, b)) (Pos (c, d)) = Pos (a + c, b + d)

toPos :: (Int, Int) -> Pos
toPos (a, b) = Pos (a, b)

fromPos :: Pos -> (Int, Int)
fromPos (Pos (a, b)) = (a, b)

dToPos :: Dir -> Pos
dToPos NORTH = Pos (-1, 0)
dToPos SOUTH = Pos (1, 0)
dToPos EAST = Pos (0, 1)
dToPos WEST = Pos (0, -1)

(.->.) :: Pos -> Dir -> Pos
(.->.) p d = p .+. dToPos d

mhdist :: Pos -> Pos -> Int 
mhdist (Pos (r, c)) (Pos (r', c')) = abs (r-r') + abs (c - c')

---

data State = State Pos Orientation deriving (Show, Ord, Eq)
instance Hashable State where
    hashWithSalt :: Int -> State -> Int
    hashWithSalt i (State p o)= hashWithSalt i (fromPos p,fromEnum o)

instance NFData State where 
  rnf (State (Pos (a,b)) o) = rnf a `seq` rnf b 
   
neighbors1 :: BSArray -> State -> [(State, Int)]
neighbors1 bsa = neighbors bsa (take 3)

neighbors2 :: BSArray -> State -> [(State, Int)]
neighbors2 bsa s = let n = neighbors bsa (take 7 . drop 3) s in n `deepseq` n 

neighbors ::BSArray -> ([(Pos, Int)] -> [(Pos, Int)]) -> State -> [(State, Int)]
neighbors bsa selector (State p o) = (first (`State` (not' o)))
    <$> concatMap (selector . scan bsa p)
        (directions (not' o))

scan :: BSArray -> Pos -> Dir -> [(Pos, Int)]
scan bsa p d = drop 1 . fromJust . sequence . takeWhile (isJust) . iterate f $ Just (p,0)
      where f (Just (p',s)) = v2s <$> BSA.lookupMaybe bsa (fromPos nextp)
              where !nextp = p' .->. d
                    v2s v =  let ss = s + ((ord v - ord '0')) in (nextp, ss)

find bsa nf h = let sn = [(State (Pos (0,0)) Vertical), (State (Pos (0,0)) Horizontal)] 
        in aStar sn hasArrived h (nf bsa )
        where hasArrived (State p _) = (p == endpos bsa)


endpos :: BSArray -> Pos
endpos bsa = Pos (BSA.rows bsa -1, BSA.cols bsa -1  )


part1 :: ByteString -> IO Integer
part1 s = do
  let bsa = makeBSarray s 
  let h (State p _) = mhdist p (endpos bsa) 
  let (Distance v) = find bsa neighbors1 h
  return . toInteger $ v

part2 :: ByteString -> IO Integer
part2 s=  do   
  let bsa = makeBSarray s 
  let h (State p _) =  mhdist p (endpos bsa) 
  let (Distance r) = find bsa neighbors2 h
  return . toInteger $ r