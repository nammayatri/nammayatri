module Core.Spec.Common.Duration where

import Beckn.Utils.GenericPretty
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

-- Describes duration as per ISO8601 format
newtype Duration = Duration Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema, PrettyShow)
