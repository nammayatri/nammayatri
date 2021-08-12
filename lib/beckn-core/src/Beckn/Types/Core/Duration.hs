module Beckn.Types.Core.Duration where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

-- Describes duration as per ISO8601 format
newtype Duration = Duration Text
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example Duration where
  example =
    Duration "2020-01-04T13:15:30Z"
