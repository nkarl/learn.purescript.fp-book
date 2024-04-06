module CSV.Rep02 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (log)

-- NOTE: input data: `<name>,<age>,<occupation>`
-- always useful to wrap the input in a named type to reduce confusion later.
newtype CSVLine = CSVLine String

derive          instance newtypeCSV :: Newtype  CSVLine _
derive newtype  instance      eqCSV :: Eq       CSVLine
derive newtype  instance    showCSV :: Show     CSVLine


-- NOTE: data structure: a product typed object
data Person = Person
  { name        :: Name
  , age         :: Int
  , occupation  :: Occupation -- Doctor | Dentist | Lawyer | Unemployed
  }

derive instance eqPerson :: Eq Person

-- Name
newtype Name = Name String

derive instance      eqName :: Eq      Name
derive instance genericName :: Generic Name _

instance showName :: Show Name where
  show (Name name) = name

-- Occupation
data Occupation = Doctor | Dentist | Lawyer | Unemployed

derive instance eqOccupation      :: Eq       Occupation
derive instance genericOccupation :: Generic  Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

-- NOTE: between the endpoints
class ToCSVLine {- some polymorphic type -} a where
  toCSV :: a -> CSVLine

instance toCSVPerson :: ToCSVLine Person where
  toCSV (Person { name, age, occupation } ) =
    CSVLine ( show name <> "," <> show age <> "," <> show occupation )

class FromCSVLine {- some polymorphic type -} a where
  fromCSV :: CSVLine -> Maybe a

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor"      -> Just Doctor
  "Dentist"     -> Just Dentist
  "Lawyer"      -> Just Lawyer
  "Unemployed"  -> Just Unemployed
  _             -> Nothing

instance fromCSVPerson :: FromCSVLine Person where
  fromCSV (CSVLine line) = do
    let  tokens = split (Pattern ",") line
    case tokens of
        [name, age, occupation]
          -> do
              age' <- fromString age
              occupation' <- toOccupation occupation
              pure $ Person { name: Name name
                            , age : age'
                            , occupation: occupation'
                            }
        _ -> Nothing

-- NOTE: unit test
test :: Effect Unit
test = do
  let
      line   = "Bobby G,25,Dentist"
      person = Person { name: Name "Bobby G"
                      , age : 25
                      , occupation: Dentist
      }
  log $ show $ toCSV person
  log $ show $ toCSV person == CSVLine line
  log $ show $ Just person == (fromCSV $ CSVLine line)
  log $ show $ Just person == (toCSV person # fromCSV)
