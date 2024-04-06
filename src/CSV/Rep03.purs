module CSV.Rep03 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (log)

-- NOTE: input data is a string of the format `{name},{age},{occupation}` for a person

-- NOTE: output is a product type
data Person = Person  { name        :: Name
                      , age         :: Int
                      , occupation :: Occupation
                      }

derive instance eqPerson :: Eq Person

-- Name
newtype Name = Name String

derive          instance newtypeName :: Newtype Name _
derive newtype  instance      eqName :: Eq      Name
derive          instance genericName :: Generic Name _

instance showName :: Show Name where
  show (Name name) = name

-- Occupation
data Occupation = Dentist | Doctor | Lawyer | Unemployed

derive instance       eqOccupation :: Eq      Occupation
derive instance  genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

-- NOTE: between two points

-- Person --> input
class ToCSV a where
  toCSV :: a -> String

instance personToCSV :: ToCSV Person where
  toCSV (Person {name, age, occupation}) =
    show name <> "," <> show age <> "," <> show occupation

-- input --> output
class FromCSV a where
  fromCSV :: String -> Maybe a

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Dentist"   -> Just Dentist
  "Doctor"    -> Just Doctor
  "Lawyer"    -> Just Lawyer
  "Unemployed"-> Just Unemployed
  _           -> Nothing

instance personFromCSV :: FromCSV Person where
  fromCSV line = do
    let
      tokens = split (Pattern ",") line
    case tokens of
        [name, age, occupation]
          -> do
            age' <- fromString age
            occupation' <- toOccupation occupation
            pure $ Person { name: Name name
                          , age: age'
                          , occupation: occupation'
                          }
        _ -> Nothing

-- NOTE: unit test
test :: Effect Unit
test = do
  let
      line    = "Bobby G,28,Lawyer"
      person  = Person { name: Name "Bobby G"
                       , age : 28
                       , occupation: Lawyer
                       }
  log $ show $ toCSV person
  log $ show $ toCSV person == line
  log $ show $ (fromCSV line) == Just person
  log $ show $ (toCSV person # fromCSV) == Just person
