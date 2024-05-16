module CSV.Rep04 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String.Common (split)
import Effect (Effect)
import Effect.Class.Console (log)

-- NOTE: input: a line string with format `{name},{age},{occupation}` per person
-- NOTE: data structure: a product type for persons
data Person
  = Person
    { name :: Name
    , age :: Int
    , occupation :: Occupation
    }

derive instance eqPerson :: Eq Person

newtype Name
  = Name String

derive instance newtypeName :: Newtype Name _

derive newtype instance eqName :: Eq Name

derive instance genericName :: Generic Name _

instance showName :: Show Name where
  show (Name name) = name

-- Occupation
data Occupation
  = Carpenter
  | Eletrician
  | Machinist
  | Trucker
  | Unemployed

derive instance eqOccupation :: Eq Occupation

derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

-- NOTE: between input and output
-- inverse
class ToCSV a where
  toCSV :: a -> String

instance personToCSV :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = show name <> "," <> show age <> "," <> show occupation

-- morphism
class FromCSV a where
  fromCSV :: String -> Maybe a

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Carpenter" -> Just Carpenter
  "Eletrician" -> Just Eletrician
  "Machinist" -> Just Machinist
  "Trucker" -> Just Trucker
  "Unemployed" -> Just Unemployed
  _ -> Nothing

instance personFromCSV :: FromCSV Person where
  fromCSV line = do
    let
      tokens = split (Pattern ",") line
    case tokens of
      [ name, age, occupation ] -> do
        age' <- fromString age
        occupation' <- toOccupation occupation
        pure
          $ Person
              { name: Name name
              , age: age'
              , occupation: occupation'
              }
      _ -> Nothing

-- NOTE: unit test
test :: Effect Unit
test = do
  let
    csvLine = "Bobby G,27,Trucker"

    person =
      Person
        { name: Name "Bobby G"
        , age: 27
        , occupation: Trucker
        }
  log $ show $ toCSV person
  log $ show $ toCSV person == csvLine
  log $ show $ Just person == fromCSV csvLine
  log $ show $ Just person == (toCSV person # fromCSV)
