module Types.Common where

import EulerHS.Prelude hiding (drop, id, state)

data GPS = GPS
  { lat :: Text,
    lon :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Address = Address
  { door :: Text,
    building :: Text,
    street :: Text,
    area :: Text,
    city :: Text,
    country :: Text,
    areaCode :: Text,
    state :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
