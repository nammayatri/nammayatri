module Beckn.Types.Core.Migration.Duration (Duration (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

-- Describes duration as per ISO8601 format
newtype Duration = Duration Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)
