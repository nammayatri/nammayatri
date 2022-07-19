module Beckn.Types.Core.Metro.Search.Address (Address (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (state)

data Address = Address
  { door :: Maybe Text,
    name :: Maybe Text,
    building :: Maybe Text,
    street :: Maybe Text,
    locality :: Maybe Text,
    ward :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    area_code :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
