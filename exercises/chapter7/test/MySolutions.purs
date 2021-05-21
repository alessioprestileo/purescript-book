module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, address)
import Data.AddressBook.Validation (Errors, matches)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
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