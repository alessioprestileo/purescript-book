module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe n1 n2 = lift2 add n1 n2

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe n1 n2 = lift2 sub n1 n2

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe n1 n2 = lift2 mul n1 n2

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe n1 n2 = lift2 div n1 n2