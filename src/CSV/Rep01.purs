module CSV.Rep01 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (log)

{--
  Data : a CSV line, containing comma-separated data about a person.
  Model: we want to convert this to a product type.
--}

-- NOTE: a CSV line/tensor: Person <name,age,occupation>
newtype CSV
  = CSV String

derive         instance newtypeCSV :: Newtype CSV _
derive newtype instance      eqCSV :: Eq      CSV
derive newtype instance    showCSV :: Show    CSV

-- NOTE: product typed data for a person
data Person
  = Person
    { name       :: String
    , age        :: Int
    , occupation :: Occupation
    }

derive instance eqPerson :: Eq Person

-- NOTE: sum typed data for possible occupations
data Occupation
  = Doctor
  | Dentist
  | Lawyer
  | Unemployed

derive instance      eqOccupation :: Eq      Occupation
derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

-- NOTE: typeclass ToCSV
class ToCSV a where
  toCSV :: a -> CSV

instance toCSVPerson :: ToCSV Person where
  toCSV ( Person { name, age, occupation } ) =
    CSV ( show name <> "," <> show age <> "," <> show occupation )

class FromCSV a where
  fromCSV :: CSV -> Maybe a

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor"     -> Just Doctor
  "Dentist"    -> Just Dentist
  "Lawyer"     -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _            -> Nothing

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV string) = do
     let  line = split (Pattern ",") string
     case line of
        [ name, age, occupation ]
          -> do
                age'        <- fromString age
                occupation' <- toOccupation occupation
                pure
                  $ Person
                      { name: name
                      , age : age'
                      , occupation: occupation'
                      }
        _ -> Nothing

-- NOTE: module's test
test :: Effect Unit
test = do
  let
    line   = "Sue Smith,23,Doctor"
    person =
      Person
        { name       : "Sue Smith"
        , age        : 23
        , occupation : Doctor
        }
  log $ show $ toCSV person
  log $ show $ toCSV person == CSV line
  log $ show $ Just person  == (fromCSV        $ CSV     line)
  log $ show $ Just person  == (toCSV   person # fromCSV     )
