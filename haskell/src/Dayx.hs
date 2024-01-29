{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wunused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Dayx (runme, runex) where

import Text.RawString.QQ

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (
  Parser,
  decimal,
  endOfInput,
  endOfLine,
  isDigit,
  many1,
  parseOnly,
  skipSpace,
  skipWhile,
 )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl', sort )
import Data.Text (Text)
import qualified Data.Text as T


import RunUtil (RunMe, runMeByteString)
import AOCHelper (readInpByteSTring)
import BSArray

example :: ByteString
example =
  [r|
|]

runex :: RunMe
runex =
  runMeByteString
    "Day x - example"
    (return example)
    part1
    (Nothing)
    part2
    (Nothing)

runme :: RunMe
runme =
  runMeByteString
    "Day x: ..."
    (readInpByteSTring "day0x.txt")
    part1
    (Nothing)
    part2
    (Nothing)

---

part1 :: ByteString -> IO Integer
part1  = undefined

part2 :: ByteString -> IO Integer
part2  = undefined
