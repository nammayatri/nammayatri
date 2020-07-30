module Beckn.Types.Core.Address where

import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Address = Address
  { _name :: Text,
    _door :: Maybe Text,
    _building :: Maybe Text,
    _street :: Maybe Text,
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
      { _name = "Address",
        _door = Nothing,
        _building = Nothing,
        _street = Nothing,
        _locality = Nothing,
        _ward = Nothing,
        _city = "BLR",
        _state = "Karnataka",
        _country = "India",
        _area_code = "560095"
      }
