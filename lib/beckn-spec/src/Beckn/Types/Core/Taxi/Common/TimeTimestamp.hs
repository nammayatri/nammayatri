module Beckn.Types.Core.Taxi.Common.TimeTimestamp where

import Kernel.Prelude

newtype TimeTimestamp = TimeTimestamp
  { timestamp :: UTCTime
  }
  deriving (Generic, Show, ToSchema, ToJSON, FromJSON)
