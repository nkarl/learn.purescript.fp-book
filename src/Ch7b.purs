module Ch7b where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

newtype CSV
  = CSV String

derive instance newtypeCSV :: Newtype CSV _

derive newtype instance Eq CSV

derive newtype instance Show CSV

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName
  = FullName String

--instance showFullName :: Show (FullName) where
--show (FullName name) = name
derive instance newtypeFullName :: Newtype FullName _

derive newtype instance showFullName :: Show FullName

derive newtype instance eqFullName :: Eq FullName

newtype Age
  = Age Int

derive instance genericAge :: Newtype Age _

derive newtype instance showAge :: Show Age

derive newtype instance eqAge :: Eq Age

data Occupation
  = Doctor
  | Dentist
  | Lawyer
  | Unemployed

derive instance genericOccupation :: Generic Occupation _

derive instance eqOccupation :: Eq Occupation

instance showOccupation :: Show Occupation where
  show = genericShow

data Person
  = Person
    { name :: FullName
    , age :: Age
    , occupation :: Occupation
    }

derive instance eqPerson :: Eq Person

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $ show name <> "," <> show age <> "," <> show occupation

class FromCSV a where
  fromCSV :: CSV -> Maybe a

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "doctor" -> Just Doctor
  "dentist" -> Just Dentist
  "lawyer" -> Just Lawyer
  "unemployed" -> Just Unemployed
  _ -> Nothing

instance fromPersonCSV :: FromCSV Person where
  fromCSV (CSV string) = case split (Pattern ",") string of
    [ name, age, occupation ] -> do
      age' <- fromString age
      occupation' <- toOccupation occupation
      pure
        $ Person
            { name: FullName name
            , age: Age age'
            , occupation: occupation'
            }
    _ -> Nothing

test :: Effect Unit
test = do
  log $ show
    $ toCSV
        ( Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            }
        )
  let
    person =
      Person
        { name: FullName "Sue Smith"
        , age: Age 23
        , occupation: Doctor
        }
  log $ show $ toCSV person == CSV "Sue Smith,23,Doctor"
  log $ show $ (toCSV person # fromCSV) == Just person
