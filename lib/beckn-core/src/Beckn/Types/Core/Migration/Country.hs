module Beckn.Types.Core.Migration.Country (Country (..)) where

import EulerHS.Prelude

data Country = Country
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
