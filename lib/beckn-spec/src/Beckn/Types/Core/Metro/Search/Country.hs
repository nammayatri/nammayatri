module Beckn.Types.Core.Metro.Search.Country (Country (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Country = Country
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
