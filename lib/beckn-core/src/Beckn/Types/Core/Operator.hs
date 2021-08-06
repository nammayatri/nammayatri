module Beckn.Types.Core.Operator where

import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Operator = Operator
  -- Core Person type
  { name :: Name,
    image :: Maybe Image,
    dob :: Maybe Text,
    organization_name :: Maybe Text,
    gender :: Maybe Text, -- male, female
    email :: Maybe Text,
    phones :: [Text], -- Phone numer in E.164 format (ITUT recommendation
    experience :: Maybe Experience
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Operator where
  example =
    Operator
      { name = example,
        image = example,
        dob = Just "28-11-1990",
        organization_name = Nothing,
        gender = Just "male",
        email = Just "john.smith@email.com",
        phones = ["+919999999999"],
        experience = example
      }

data Experience = Experience
  { label :: Text,
    value :: Text,
    unit :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Experience where
  example =
    Experience
      { label = "Senior",
        value = "8",
        unit = "years"
      }
