module Beckn.Types.Core.Schedule where

import Beckn.Utils.JSON
import Data.Text
import Data.Time
import EulerHS.Prelude

data Schedule = Schedule
  { day :: Text, -- "MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"
    slots :: [Slot]
  }
  deriving (Generic, Show)

instance FromJSON Schedule where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Schedule where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Slot = Slot
  { open :: UTCTime,
    close :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Slot where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Slot where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
