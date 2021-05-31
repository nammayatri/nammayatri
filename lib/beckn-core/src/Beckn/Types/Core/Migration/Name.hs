module Beckn.Types.Core.Migration.Name where

import EulerHS.Prelude

newtype Name = Name Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
