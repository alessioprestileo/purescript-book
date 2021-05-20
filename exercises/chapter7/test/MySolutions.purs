module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe)

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