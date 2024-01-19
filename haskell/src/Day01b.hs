{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day01b (run) where

import AOCHelper
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.Maybe ( fromMaybe, fromJust, catMaybes, mapMaybe )
import Data.Text (Text)
import qualified Data.Text as T
-- import Data.List

digwords :: [Text]
digwords = map T.pack ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

fromDigiwords :: [Text] -> Int -> Text -> Maybe Int
fromDigiwords [] _ _ = Nothing
fromDigiwords (x : xs) n s
        | x `T.isPrefixOf` s = Just n
        | otherwise = fromDigiwords xs (n + 1) s

fdd :: Bool -> Text -> Maybe Int
fdd cw t | t == T.empty = Nothing
         | isDigit x = Just . digitToInt $ x
         | cw = fromDigiwords digwords 1 t
         | otherwise = Nothing

    where (x,_) =fromJust (T.uncons t)


fd :: Bool -> Text -> Int
fd cw s = (head . mapMaybe (fdd cw) $ T.tails s )* 10 + (head . mapMaybe (fdd cw) $ reverse (T.tails s) )

run :: IO ()
run = do
    putStrLn "--- Day 1: Trebuchet?! ---"
    putStr " Part1: "
    readInpT "day01.txt" >>= part1 >>= assertInt 55386
    putStr " Part2: "
    readInpT "day01.txt" >>= part2 >>= assertInt 54824

part1 :: Text -> IO Int
part1 s = do
    return $ sum $ map (fd False) $ T.lines s

part2 :: Text -> IO Int
part2 s = do
    return $ sum $ map (fd True) $ T.lines s
