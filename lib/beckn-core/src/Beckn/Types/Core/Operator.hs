module Beckn.Types.Core.Operator where

import Beckn.Types.Core.Person
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Operator = Operator
  -- Core Person type
  { _name :: Name,
    _image :: Maybe Image,
    _dob :: Maybe Text,
    _organization_name :: Maybe Text,
    _gender :: Maybe Text, -- male, female
    _email :: Maybe Text,
    _phones :: [Text], -- Phone numer in E.164 format (ITUT recommendation
    _experience :: Maybe Experience
  }
  deriving (Generic, Show)

instance FromJSON Operator where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Operator where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Operator where
  example =
    Operator
      { _name = example,
        _image = example,
        _dob = Just "28-11-1990",
        _organization_name = Nothing,
        _gender = Just "male",
        _email = Just "john.smith@email.com",
        _phones = ["+919999999999"],
        _experience = example
      }

data Experience = Experience
  { _label :: Text,
    _value :: Text,
    _unit :: Text
  }
  deriving (Generic, Show)

instance FromJSON Experience where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Experience where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Experience where
  example =
    Experience
      { _label = "Senior",
        _value = "8",
        _unit = "years"
      }
