module Beckn.Types.Core.Address where

import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Address = Address
  { _name :: Maybe Text,
    _door :: Text,
    _building :: Maybe Text,
    _street :: Text,
    _locality :: Maybe Text,
    _ward :: Maybe Text,
    _city :: Text,
    _state :: Text,
    _country :: Text,
    _area_code :: Text -- Area code. This can be Pincode, ZIP code or any equivalent
  }
  deriving (Generic, Show)

instance FromJSON Address where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Address where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Address where
  example =
    Address
      { _name = Just "Address",
        _door = "#817",
        _building = Just "Juspay Apartments",
        _street = "27th Main",
        _city = "Bangalore",
        _state = "Karnataka",
        _country = "India",
        _area_code = "560047",
        _locality = Just "8th Block Koramangala",
        _ward = Nothing
      }
