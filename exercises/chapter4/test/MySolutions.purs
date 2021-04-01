module Test.MySolutions where

import Prelude

import Data.Array (head, tail, filter)
import Data.Maybe (fromMaybe)

isEven :: Int -> Boolean
isEven n = case n of
  0 -> true
  1 -> false
  _ -> isEven $ n - 2

oneIfEven :: Int -> Int
oneIfEven n = if isEven n then 1 else 0

countEven :: Array Int -> Int
countEven ints = safelyCountEven ints 0
  where
  safelyCountEven :: Array Int -> Int -> Int
  safelyCountEven [] count = count
  safelyCountEven ints' count = safelyCountEven (safeTail ints') updatedCount
    where
    safeTail :: forall a. Array a -> Array a
    safeTail xs = fromMaybe [] $ tail xs
    updatedCount = add count increment
    increment = oneIfEven $ safeHead ints'
    safeHead :: Array Int -> Int
    safeHead xs = fromMaybe (-1) $ head xs

squared :: Array Number -> Array Number
squared = map (\n -> n * n)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)