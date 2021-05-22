module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneType(..), address, phoneNumber)
import Data.AddressBook.Validation (Errors, matches)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (foldMap, traverse)
import Data.Validation.Semigroup (V)

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 add

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 mul

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 sub

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 div

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 add

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 mul

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 sub

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 div

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = map Just x
combineMaybe Nothing = pure Nothing

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "[^\\s]" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved addr =
  address <$> matches "Street" nonEmptyRegex addr.street
          <*> matches "City" nonEmptyRegex addr.city
          <*> matches "State" stateRegex addr.state

validateAddressImprovedAdo :: Address -> V Errors Address
validateAddressImprovedAdo addr = ado
  street <- matches "Street" nonEmptyRegex addr.street
  city   <- matches "City" nonEmptyRegex addr.city
  state  <- matches "State" stateRegex addr.state
  in address street city state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t