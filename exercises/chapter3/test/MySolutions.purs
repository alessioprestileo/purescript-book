module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook, findEntry)
import Data.List (filter, head, nubBy)
import Data.Maybe (Maybe(..))

genericCompare :: forall a b. Eq b => (a -> b) -> b -> (a -> Boolean) 
genericCompare accessor target = (==) target <<< accessor

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter (genericCompare _.address.street street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = case found of
  Just entry -> true
  Nothing -> false
  where found = findEntry firstName lastName book

isDuplicateAddress :: Entry -> Entry -> Boolean
isDuplicateAddress first second = (first.firstName == second.firstName) && (first.lastName == second.lastName)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates addressBook = nubBy isDuplicateAddress addressBook