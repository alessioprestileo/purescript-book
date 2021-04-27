module Test.MySolutions where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over2, wrap)

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

derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  add = over2 Complex add
  zero = wrap zero
  mul = over2 Complex
    \ { real: r1, imaginary: i1} { real: r2, imaginary: i2}
    ->
      { real: r1 * r2 - i1 * i2, imaginary: r1 * i2 + i1 * r2}
  one = wrap one

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String


derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

derive instance genericNonEmpty :: Generic (NonEmpty a) _

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show = genericShow

derive instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [e2] <> a2)