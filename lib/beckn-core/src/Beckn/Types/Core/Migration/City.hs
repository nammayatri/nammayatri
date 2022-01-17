module Beckn.Types.Core.Migration.City (City (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data City = City
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
