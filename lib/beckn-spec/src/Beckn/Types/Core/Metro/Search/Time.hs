module Beckn.Types.Core.Metro.Search.Time where

import Beckn.Types.Core.Metro.Search.Duration (Duration)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Time = Time
  { label :: Maybe Text,
    timestamp :: Maybe UTCTime,
    duration :: Maybe Duration,
    range :: Maybe TimeRange,
    days :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

emptyTime :: Time
emptyTime =
  Time
    { label = Nothing,
      timestamp = Nothing,
      duration = Nothing,
      range = Nothing,
      days = Nothing
    }

data TimeRange = TimeRange
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
