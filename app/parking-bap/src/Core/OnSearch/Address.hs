module Core.OnSearch.Address (Address (..)) where

import Beckn.Prelude

data Address = Address
  { name :: Text,
    street_address :: Text,
    locality :: Text,
    city :: Maybe Text,
    state :: Text,
    country :: Text,
    area_code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
