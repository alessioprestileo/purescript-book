module Test.MySolutions where

import Prelude
import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head)
import Data.Maybe (Maybe)

findEntryByStreet:: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterStreet where
  filterStreet :: Entry -> Boolean
  filterStreet entry = entry.address.street == street