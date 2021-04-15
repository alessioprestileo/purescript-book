module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Person (Person)
import Data.Picture (Shape(..), Point, getCenter, origin)

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

centerShape :: Shape-> Shape
centerShape (Circle c r) = Circle origin r
centerShape (Rectangle center width height) = Rectangle origin width height
centerShape line@(Line p1 p2) = let
  delta = getCenter line in
  Line (p1 - delta) (p2 - delta)
centerShape (Text center str) = Text origin str

scaleShape :: Number -> Shape -> Shape
scaleShape n (Circle c r) = Circle c (r * n)
scaleShape n (Rectangle c w h) = Rectangle c (w * n) (h * n)
scaleShape n (Line p1 p2) = let
  scale :: Point
  scale = {x: n, y: n}
  in
  Line (p1 * scale) (p2 * scale)
scaleShape n text = text

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText :: Shape -> Maybe String
shapeText (Text c str) = Just str
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)