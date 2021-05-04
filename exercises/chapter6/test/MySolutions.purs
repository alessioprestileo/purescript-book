module Test.MySolutions where

import Prelude

import Data.Array (length, nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Hashable (class Hashable, hashEqual)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, overF, wrap)

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

instance functorNonEmpty :: Functor NonEmpty where
  map fn (NonEmpty e a) = NonEmpty (fn e) (map fn a)

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a1) (Finite a2) = compare a1 a2

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr fn start (NonEmpty elem array) = fn elem (foldr fn start array)
  foldl fn start (NonEmpty elem array) = foldl fn (fn start elem) array
  foldMap fn (NonEmpty elem array) = (fn elem) <> foldMap fn array
  
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldr fn start (OneMore val more) = fn val (foldr fn start more)
  foldl fn start (OneMore val more) = foldl fn (fn start val) more
  foldMap fn (OneMore val more) = (fn val) <> foldMap fn more

derive instance eqPoint :: Eq Point
derive instance eqShape :: Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs = case maximum xs of
  Just m -> m

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply n) k = n * k


instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) s = power s n


instance actionArray :: Action m a => Action m (Array a) where
  act m xs = map (act m) xs

newtype Self m = Self m

derive newtype instance showMultiply :: Show Multiply
derive newtype instance eqMultiply :: Eq Multiply
derive newtype instance showSelf :: Show a => Show (Self a)
derive newtype instance eqSelf :: Eq a => Eq (Self a)

instance actionSelf :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates xs = length xs > length (nubByEq eqCheck xs) where
  eqCheck :: a -> a -> Boolean
  eqCheck a1 a2 = a1 == a2 && hashEqual a1 a2