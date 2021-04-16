module Test.MySolutions where

import Prelude

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"