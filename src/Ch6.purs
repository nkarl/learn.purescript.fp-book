module Ch6 (test) where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, (<<<), (+), (-), (<), (>), (>=), (/=), (==), show, discard, negate, otherwise, type (~>))

{--
  Chapter 6:
    - Problem Description:
      - We have a Type Alias Address with a function to get address
          getDirections :: Address -> Directions
      - We have a few Data Types containing Address as attribute
    - Questions:
      - How would we get Directions for each of these types

--}

-- We have a type alias Address with a given Record
type Address
  = { street1 :: String
    , street2 :: String
    , city :: String
    , state :: String
    , zip :: String
    }

-- as well as Data Types
data Person = Person {
  name :: String,
  age :: Int,
  address :: Address
}

data Company = Company {
  name :: String,
  address :: Address
}

data Residence = Home Address | Facility Address

test :: Effect Unit
test = do
  log $ show "Ch6 tests"
