module Core.Address (Address (..)) where

import Beckn.Prelude --hiding (state)

data Address = Address
  { name :: Text,
    street_address :: Text,
    locality :: Text,
    city :: Maybe Text,
    state :: Text,
    country :: Text,
    area_code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
