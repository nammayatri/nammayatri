module Beckn.Types.Core.Schedule where

import Data.Text
import Data.Time
import EulerHS.Prelude

data Schedule = Schedule
  { _day :: Text, -- "MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"
    _slots :: [Slot]
  }
  deriving (Generic, Show)

instance FromJSON Schedule where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Schedule where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Slot = Slot
  { _open :: UTCTime,
    _close :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Slot where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Slot where
  toJSON = genericToJSON stripAllLensPrefixOptions
