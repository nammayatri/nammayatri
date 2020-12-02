module Beckn.Types.Core.Migration.Gps (Gps (..)) where

import EulerHS.Prelude

newtype Gps = Gps Text
  deriving (Generic, Show, FromJSON, ToJSON)
