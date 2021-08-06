module Beckn.Types.Core.Migration.Schedule where

import Beckn.Types.Core.Migration.Duration
import Data.Time
import EulerHS.Prelude

data Schedule = Schedule
  { frequency :: Maybe Duration,
    holidays :: Maybe [UTCTime],
    times :: Maybe [UTCTime]
  }
  deriving (Generic, FromJSON, ToJSON, Show)
