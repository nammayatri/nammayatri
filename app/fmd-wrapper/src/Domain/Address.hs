module Domain.Address where

import Beckn.Prelude
import Beckn.Types.Id

data Address = Address
  { id :: Id Address,
    lat :: Double,
    lon :: Double,
    country :: Text,
    state :: Text,
    city :: Text,
    street :: Text,
    building :: Maybe Text,
    door :: Text,
    name :: Maybe Text,
    pincode :: Text,
    instructions :: Maybe Text
  }
  deriving (Generic)

data Stop = Pickup | Drop
