module Beckn.Types.Core.Schedule where

import Data.Text
import Data.Time
import EulerHS.Prelude

data Schedule = Schedule
  { day :: Text, -- "MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"
    slots :: [Slot]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Slot = Slot
  { open :: UTCTime,
    close :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)
