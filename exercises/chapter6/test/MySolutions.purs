module Test.MySolutions where

import Prelude

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex = Complex
  { real:: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) =
    let 
      optionalPlus
        | imaginary >= 0.0 = "+"
        | otherwise = ""
    in
      show real <> optionalPlus <> show imaginary <> "i"

derive instance eqComplex :: Eq Complex