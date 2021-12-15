module Beckn.Types.Core.Taxi.Search.Address where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)

data Address = Address
  { door :: Maybe Text,
    area :: Maybe Text,
    building :: Maybe Text,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    area_code :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example Address where
  example =
    Address
      { door = Nothing,
        area = Nothing,
        building = Nothing,
        street = Nothing,
        city = Nothing,
        state = Nothing,
        country = Nothing,
        area_code = Nothing
      }
