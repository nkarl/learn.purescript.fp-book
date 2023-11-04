module Ch6bad where

import Ch6
import Prelude

class HasAddress a where
  getAddress :: a -> Address

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person person) = person.address

instance hasAddressCompany :: HasAddress Company where
  getAddress (Company company) = company.address

instance hasAddressResidence :: HasAddress Residence where
  getAddress = case _ of
    Home address -> address
    Facility address -> address

instance hasAddressEmptyLot :: HasAddress EmptyLot where
  getAddress (EmptyLot lot) = lot.address


getDirections :: forall a. Show HasAddress a => a -> Directions
getDirections hasAddr = let address = getAddress hasAddr in ...