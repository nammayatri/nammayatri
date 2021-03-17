module Beckn.Types.Mobility.Traveller where

import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Traveller = Traveller
  -- Core Person type
  { _name :: Name,
    _image :: Maybe Image,
    _dob :: Maybe Text,
    _organization_name :: Maybe Text,
    _gender :: Text, -- male, female
    _email :: Maybe Text,
    _phones :: [Text], -- Phone numer in E.164 format (ITUT recommendation,
    _origin_stop_id :: Text,
    _destination_stop_id :: Text
  }
  deriving (Generic, Show)

instance FromJSON Traveller where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Traveller where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Traveller where
  example =
    Traveller
      { _name = example,
        _image = example,
        _dob = Just "28-11-1990",
        _organization_name = Nothing,
        _gender = "male",
        _email = Just "john.smith@email.com",
        _phones = ["+919999999999"],
        _origin_stop_id = idExample,
        _destination_stop_id = idExample
      }
