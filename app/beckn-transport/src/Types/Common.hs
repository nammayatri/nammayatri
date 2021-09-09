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

data ProviderInfo = ProviderInfo
  { id :: Text,
    name :: Text,
    stats :: Text,
    contacts :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data ProviderStats = ProviderStats
  { completed :: Maybe Int,
    inprogress :: Maybe Int,
    confirmed :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)