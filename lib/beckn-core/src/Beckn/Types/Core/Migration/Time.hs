module Beckn.Types.Core.Migration.Time where

import Beckn.Types.Core.Migration.Duration (Duration)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Time = Time
  { label :: Maybe Text,
    timestamp :: Maybe UTCTime,
    duration :: Maybe Duration,
    range :: Maybe Range,
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

data Range = Range
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
