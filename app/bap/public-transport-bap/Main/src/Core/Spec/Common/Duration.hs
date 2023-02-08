module Core.Spec.Common.Duration where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Utils.GenericPretty

-- Describes duration as per ISO8601 format
newtype Duration = Duration Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema, PrettyShow)
