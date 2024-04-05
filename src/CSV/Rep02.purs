module CSV.Rep02 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

-- NOTE: CSV table: Person <name,age,occupation>
newtype CSV
  = CSV String

derive instance newtypeCSV :: Newtype CSV _

derive newtype instance eqCSV :: Eq CSV

derive newtype instance showCSV :: Show CSV

-- NOTE: sum typed data for possible occupations
data Occupation
  = Doctor
  | Dentist
  | Lawyer
  | Unemployed

derive instance genericOccupation :: Generic Occupation _

derive instance eqOccupation :: Eq Occupation

instance showOccupation :: Show Occupation where
  show = genericShow

-- NOTE: product typed data for a person
data Person
  = Person
    { name :: String
    , age :: Int
    , occupation :: Occupation
    }

derive instance eqPerson :: Eq Person

-- NOTE: typeclass ToCSV
class ToCSV a where
  toCSV :: a -> CSV

instance toCSVPerson :: ToCSV Person where
  toCSV ( Person { name, age, occupation }
  ) = CSV $ show name <> "," <> show age <> "," <> show occupation

test :: Effect Unit
test = do
  let
    person =
      Person
        { name: "Sue Smith,23,Doctor"
        , age: 23
        , occupation: Doctor
        }
  log $ show $ toCSV person
  log $ show $ toCSV person == CSV "Sue Smith,23,Doctor"
