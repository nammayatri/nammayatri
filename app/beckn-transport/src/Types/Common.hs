module Types.Common where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (drop, id, state)

data Address = Address
  { door :: Maybe Text,
    building :: Maybe Text,
    street :: Maybe Text,
    area :: Maybe Text,
    city :: Maybe Text,
    country :: Maybe Text,
    areaCode :: Maybe Text,
    state :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

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
