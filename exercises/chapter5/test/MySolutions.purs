module Test.MySolutions where

import Prelude

import Data.Person (Person)
import Data.Picture (Shape(..), origin)

factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
             | otherwise = factorial n / (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n-1) k + pascal (n-1) (k-1)

sameCity :: Person -> Person -> Boolean
sameCity {address: {city: c1}} {address: {city: c2}} = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton default [val] = val
fromSingleton default _ = default

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0