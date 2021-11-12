module Beckn.Types.Core.Migration.Image (Image (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Image = Image Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- TODO: functions to work with different formats
-- https://raw.githubusercontent.com/beckn/protocol-specifications/core-v0.9.1/core/v0/schema/image.json
