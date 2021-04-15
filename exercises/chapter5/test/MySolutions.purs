module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Person (Person)
import Data.Picture (Bounds, Picture, Point, Shape(..), bounds, getCenter, intersect, origin, shapeBounds) as DP
import Math as Math

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

circleAtOrigin :: DP.Shape
circleAtOrigin = DP.Circle DP.origin  10.0

centerShape :: DP.Shape -> DP.Shape
centerShape (DP.Circle c r) = DP.Circle DP.origin  r
centerShape (DP.Rectangle center width height) = DP.Rectangle DP.origin  width height
centerShape line@(DP.Line p1 p2) = let
  delta = DP.getCenter line in
  DP.Line (p1 - delta) (p2 - delta)
centerShape (DP.Text center str) = DP.Text DP.origin  str

scaleShape :: Number -> DP.Shape -> DP.Shape
scaleShape n (DP.Circle c r) = DP.Circle c (r * n)
scaleShape n (DP.Rectangle c w h) = DP.Rectangle c (w * n) (h * n)
scaleShape n (DP.Line p1 p2) = let
  scale :: DP.Point 
  scale = {x: n, y: n}
  in
  DP.Line (p1 * scale) (p2 * scale)
scaleShape n text = text

doubleScaleAndCenter :: DP.Shape -> DP.Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText :: DP.Shape -> Maybe String
shapeText (DP.Text c str) = Just str
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

area :: DP.Shape -> Number
area (DP.Circle _ r) = Math.pi * (Math.pow r 2.0)
area (DP.Rectangle _ w h) = w * h
area _ = 0.0

data ShapeExt = Shape DP.Shape | Clipped DP.Picture DP.Point Number Number

shapeBounds :: ShapeExt -> DP.Bounds
shapeBounds (Shape shape) = DP.shapeBounds shape
shapeBounds (Clipped pic c w h) = DP.intersect (DP.bounds pic) $ DP.shapeBounds (DP.Rectangle c w h)