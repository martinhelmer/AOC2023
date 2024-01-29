{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day10 (runme, runex) where

import Text.RawString.QQ

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (mapMaybe, fromJust)

import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import qualified BSArray as BS
import qualified Data.HashSet as S
import Debug.Trace (trace)
import Data.List (foldl')

type MySet = S.HashSet (Int,Int)

example :: ByteString
example =
  [r|7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
|]

runex :: RunMe
runex =
  runMeByteString
    "Day 10 - example"
    (return example)
    part1
    (Just 8)
    part2
    (Just 1)

runme :: RunMe
runme =
  runMeByteString
    "Day 10: Pipe Maze (TODO: try intset)"
    (readInpByteSTring "day10.txt")
    part1
    (Just 7005)
    part2
    (Just 417)

---
newtype Pos = Pos (Int, Int) deriving (Eq, Show)
data Dir = NORTH | EAST | SOUTH | WEST deriving (Eq, Show)

(.+.) :: Pos -> Pos -> Pos
(.+.) (Pos (a,b)) (Pos (c,d)) = Pos (a+c, b+d)

toPos :: (Int, Int) -> Pos
toPos (a,b) = Pos (a,b)

fromPos (Pos (a,b)) = (a, b)

dToPos :: Dir -> Pos
dToPos NORTH = Pos (-1,0)
dToPos SOUTH = Pos (1, 0)
dToPos EAST = Pos (0,1)
dToPos WEST = Pos (0,-1)

(.->.) :: Pos -> Dir -> Pos
(.->.) p d = p .+. dToPos d
-- 


turn :: Dir -> Char -> Maybe Dir
turn d c = case (d, c) of
  (NORTH, '|') -> Just NORTH
  (SOUTH, '|') -> Just SOUTH
  (WEST,  '-') -> Just WEST
  (EAST,  '-') -> Just EAST
  (EAST,  '7') -> Just SOUTH
  (NORTH, '7') -> Just WEST
  (NORTH, 'F') -> Just EAST
  (WEST,  'F') -> Just SOUTH
  (SOUTH, 'L') -> Just EAST
  (WEST,  'L') -> Just NORTH
  (EAST,  'J') -> Just NORTH
  (SOUTH, 'J') -> Just WEST
  _ -> Nothing


turnandmove :: Char -> (Pos, Dir) -> Maybe (Pos, Dir)
turnandmove 'S' (p, d) = Just  (p .->. d , d)
turnandmove c (p,d) =  (\d' -> (p .->. d' , d')) <$> turn d c

loop :: BS.BSArray -> [Pos] -> Pos -> Dir -> Maybe [Pos]
loop bs l p d
      | c == 'S' && l /= [] = Just l
      | otherwise = case turnandmove c (p,d) of
      Nothing -> Nothing
      Just (p',d') ->  loop bs (p:l) p' d'
  where c = BS.lookup bs . fromPos $ p

looplist :: BS.BSArray -> Pos -> [Pos]
looplist bs start = head . mapMaybe (loop bs [] start) $ [NORTH, EAST, SOUTH, WEST]

part1 :: ByteString -> IO Integer
part1 s = do
  let bs = BS.makeBSarray s
  let start = toPos . fromJust . BS.elemIndex bs $ 'S'
  return . toInteger
         . flip div 2
         . length
         $ looplist bs start

countrow :: Bool -> MySet -> Int -> ByteString -> Int
countrow sflips m rownum s = fst . foldl' f (0, False) $ zip [0..] (B.unpack s)
  where f (count, isinside) (colnum, c) =
          if (rownum, colnum) `S.member` m
            then (count, if flipit then not isinside else isinside)
            else (if isinside then count+1 else count, isinside)
          where flipit = c == '|'
                      || c == '7'
                      || (c == 'S' && sflips)
                      || c == 'F'

part2 :: ByteString -> IO Integer
part2 s = do
    let bs = BS.makeBSarray s
    let start = toPos . fromJust . BS.elemIndex bs $ 'S'
    let looppoints = S.fromList . map fromPos $ looplist bs start
    let sflips = fromPos (start .->. SOUTH) `S.member` looppoints

    return . toInteger
           . sum
           . map (\rownum -> countrow sflips looppoints rownum (BS.row bs rownum))
           $  [0..(BS.rows bs -1)]
