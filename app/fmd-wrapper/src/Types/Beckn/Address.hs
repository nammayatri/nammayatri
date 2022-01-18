module Types.Beckn.Address (Address (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (state)

data Address = Address
  { door :: Text,
    name :: Maybe Text,
    building :: Maybe Text,
    street :: Text,
    city :: Text,
    state :: Text,
    country :: Text,
    area_code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
