module Domain.Types.LocationAddress where

import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)

data LocationAddress = LocationAddress
  { street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show, Eq, PrettyShow)
