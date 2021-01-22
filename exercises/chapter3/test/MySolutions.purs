module Test.MySolutions where

import Prelude
import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head)
import Data.Maybe (Maybe)

genericCompare :: forall a b. Eq b => (a -> b) -> b -> (a -> Boolean) 
genericCompare accessor target = (==) target <<< accessor

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter (genericCompare _.address.street street)
