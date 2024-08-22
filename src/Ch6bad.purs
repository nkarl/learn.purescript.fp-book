module Ch6bad where

import Ch6
import Prelude

data HasAddress
  = AddressPerson Person
  | AddressCompany Company
  | AddressResidence Residence
  | AddressEmptyLot EmptyLot

newtype Miles = Miles Int

getDirections :: Address -> Directions
getDirections address = Directions (address)

getDirectionsPerson :: Person -> Directions
getDirectionsPerson (Person p) = getDirections p.address

getDirectionsCompany :: Company -> Directions
getDirectionsCompany (Company c) = getDirections c.address

getDirectionsResidence :: Residence -> Directions
getDirectionsResidence =
  getDirections
    <<< case _ of
      Home address -> address
      Facility address -> address

getDirectionsEmptyLot :: EmptyLot -> Directions
getDirectionsEmptyLot (EmptyLot lot) = getDirections lot.address

{--
  Every time we add a new Address data type, we have to implement a new
  version of getDirections for that type. It's too much boilerplate.
--}
getDirectionsTo :: HasAddress -> Directions
getDirectionsTo = case _ of
  AddressPerson person -> getDirectionsPerson person
  AddressCompany company -> getDirectionsCompany company
  AddressResidence residence -> getDirectionsResidence residence
  AddressEmptyLot lot -> getDirectionsEmptyLot lot

{--
  Worse yet, if we need to add a new function to calculate Miles from Address,
  we HAVE TO write a new function for each type in Has Address.
--}
{--
getMilesTo :: HasAddress -> Miles
getMilesTo = case _ of
  AddressPerson person -> getMilesPerson person
  AddressCompany company -> getMilesCompany company
  AddressResidence residence -> getMilesResidence residence
  AddressEmptyLot lot -> getMilesEmptyLot lot
--}
{--
  Instead of so much labor, we write a single getMiles function
--}
getMiles :: Address -> Miles
getMiles address = Miles 0 -- implementation not important here so we always return 0

getMilesTo :: HasAddress -> Miles
getMilesTo =
  getMiles
    <<< case _ of
      AddressPerson (Person { address }) -> address
      AddressCompany (Company { address }) -> address
      AddressResidence (Home address) -> address
      AddressResidence (Facility address) -> address
      AddressEmptyLot (EmptyLot { address }) -> address

{--
  This version despite being better, is still a pain because we have to maintain 
  the sum tpye HasAddress and update the case expression for both
    - getDirectionsTo
    - getMilesTo
--}
