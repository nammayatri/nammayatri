module Types.Common where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (drop, id, state)

data GPS = GPS
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

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
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)