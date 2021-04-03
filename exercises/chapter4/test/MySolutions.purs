module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (cons, filter, foldl, head, length, tail, (..))
import Data.Int (quot, rem)
import Data.Maybe (fromMaybe)
import Prim.Boolean (True)
import Test.Examples (factors)

infix 4 filter as <$?>

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

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = (_ >= 0.0) <$?> xs

isPrime :: Int -> Boolean
isPrime n = n > 1 && length (factors n) == 1

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i * i + j * j == k * k
  pure [i, j, k]

factorize :: Int -> Array Int
factorize 0 = []
factorize n = factorize' 2 n []
  where
  factorize' :: Int -> Int -> Array Int -> Array Int
  factorize' _ 1 result = result
  factorize' divisor dividend result =
    let
      remainder = rem dividend divisor
    in
      if remainder == 0 then
        factorize' divisor (quot dividend divisor) (cons divisor result)
      else
        factorize' (divisor + 1) dividend result

allTrue ::  Array Boolean -> Boolean
allTrue = foldl (\acc bool -> acc && bool) true