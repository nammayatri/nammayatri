module Beckn.Types.Core.Migration.GPS (GPS (..)) where

import EulerHS.Prelude

newtype GPS = GPS Text
  deriving (Generic, Show, FromJSON, ToJSON)
