module Main (main) where
import System.Environment
import Numeric (showFFloat)
import Day01
import Day01b

import Control.Monad
import System.TimeIt

sToR :: String -> IO ()
sToR "ALL" = forM_ [timeIt Day01.run]  id
sToR "01" = Day01.run
sToR "01b" = Day01b.run
-- sToR "02" = Day02.run
-- sToR "03" = Day03.run
-- sToR "04" = Day04.run
-- sToR "05" = Day05.run
-- sToR "06" = Day06.run
-- sToR "07" = Day07.run
-- sToR "07b" = Day07b.run
-- sToR "08" = Day08.run
-- sToR "09" = Day09.run
-- sToR "10" = Day10.run
-- sToR "11" = Day11.run
-- sToR "12" = Day12.run
-- sToR "13" = Day13.run
-- sToR "14" = Day14.run
-- sToR "15" = Day15.run
-- sToR "16" = Day16.run
-- sToR "17" = Day17.run
-- sToR "18" = Day18.run
-- sToR "19" = Day19.run
-- sToR "20" = Day20.run
-- sToR "22" = Day22.run
-- sToR "23" = Day23.run
-- sToR "24" = Day24.run
-- sToR "24b" = Day24b.run
-- sToR "24c" = Day24c.run


sToR _ = undefined

f :: [String] -> IO (Double, ())
f args = timeItT ( sToR $ head args) 

main :: IO ()
main = do
    args <- getArgs
    f args  >>= (\s -> print (1000000.0 * fst s) )
