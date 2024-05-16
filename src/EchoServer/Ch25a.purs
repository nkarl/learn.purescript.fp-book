module EchoServer.Ch25a where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

{--
    NOTE: DATA STRUCTURES
--}
type {-- alias --} Personal
  = { height :: Number
    , weight :: Number
    , age :: Int
    }

newtype GPA
  = GPA Number

type {-- alias --} Student  -- to be sent to the EchoServer
  = { grade :: Level
    , gpa :: GPA
    , teacher :: Teacher
    , personal :: Personal
    }

data Level
  = Preschool
  | Kindergarten
  | Level Int
  | College Int

type {-- alias --} Teacher  -- to be sent to the EchoServer
  = { levels :: Array Level
    , students :: Int
    , personal :: Personal
    , status :: ProfessionalStatus
    }

data ProfessionalStatus
  = Student
  | Probabationary
  | NonTenured
  | Tenured

{--
    NOTE: Instances
--}
test :: Effect Unit
test = do
  log $ show "placeholder"
