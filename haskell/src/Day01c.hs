{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

module Day01c (run, runquiet, runme) where

import AOCHelper
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char (ord)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, mapMaybe, fromJust)

import RunUtil (RunMe, runMeByteString)
runme :: RunMe
runme = runMeByteString 
        "Day 1: (BS) Trebuchet?!" 
        (readInpByteSTring "day01.txt") 
        (fmap toInteger . part1) 
        (Just 55386) 
        (fmap toInteger . part2) 
        (Just 54824)

---------


dw :: V.Vector ByteString
dw =  V.fromList ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

dwr :: V.Vector ByteString
dwr =V.fromList . map B.reverse $ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

fromWords ::  V.Vector ByteString -> Int -> ByteString -> Maybe Int
fromWords _ 10 _ = Nothing
fromWords v n s
    | (v V.! (n-1)) `B.isPrefixOf` s = Just n
    | otherwise = fromWords v (n + 1) s

myisprefixof :: ByteString -> ByteString -> Bool
myisprefixof a b | B.length a == 0 = True 
                 | B.length b == 0 = False 
                 | a' /= b' = False 
                 | otherwise  = myisprefixof ax bx 
    where (a', ax) = fromJust . B.uncons $ a
          (b', bx) = fromJust . B.uncons $ b 

fdd :: V.Vector ByteString -> Bool -> ByteString -> Maybe Int
fdd v check_words t =
    let
        x = ord (B.head t) - ord '0'
        go
            | t == B.empty = Nothing
            | 0 <= x && x <= 9 = Just x
            | check_words = fromWords v 1 t
            | otherwise = Nothing
     in
        go

revtails :: ByteString -> [ByteString]
revtails bs = map (`B.takeEnd` bs) [1 ..]
-- revtails = reverse . B.tails

-- ^ ^ this is obviously slower

getfirst :: (ByteString -> Maybe Int) -> [ByteString] -> Int
getfirst f (x : xs) = fromMaybe (getfirst f xs) (f x)

fd :: Bool -> ByteString -> Int
fd cw s = (head . mapMaybe (fdd dw cw) $ B.tails s) * 10 + (head . mapMaybe (fdd dwr cw) $ B.tails (B.reverse s))

-- fd :: Bool -> ByteString -> Int
-- fd cw s = (head . mapMaybe (fdd cw digwords) $ B.tails s) * 10 + (head . mapMaybe (fdd cw (map B.reverse digwords)) $ B.tails $ B.reverse s)


-- fd cw s = getfirst (fdd cw) (B.tails s) * 10 + getfirst (fdd cw) (revtails s)
-- this is surprisingly slower

run :: IO ()
run = do
    putStrLn "--- Day 1: Trebuchet?! ---"
    putStr " Part1: "
    f <- readInpByteSTring "day01.txt"
    part1 f >>= assertInt 55386
    putStr " Part2: "
    part2 f >>= assertInt 54824

runquiet :: IO ()
runquiet = do
    f <- readInpByteSTring "day01.txt"
    part1 f >>= assertInt 55386
    part2 f >>= assertInt 54824

part1 :: ByteString -> IO Int
part1 s = do
    return $ sum $ map (fd False) $ B.lines s

part2 :: ByteString -> IO Int
part2 s = do
    return $ sum $ map (fd True) $ B.lines s
