module Beckn.Types.Core.Address where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text
import EulerHS.Prelude hiding (state)

data Address = Address
  { name :: Maybe Text,
    door :: Text,
    building :: Maybe Text,
    street :: Text,
    locality :: Maybe Text,
    ward :: Maybe Text,
    city :: Text,
    state :: Text,
    country :: Text,
    area_code :: Text -- Area code. This can be Pincode, ZIP code or any equivalent
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Address where
  example =
    Address
      { name = Just "Address",
        door = "#817",
        building = Just "Juspay Apartments",
        street = "27th Main",
        city = "Bangalore",
        state = "Karnataka",
        country = "India",
        area_code = "560047",
        locality = Just "8th Block Koramangala",
        ward = Nothing
      }
