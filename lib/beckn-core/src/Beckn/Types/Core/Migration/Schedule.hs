module Beckn.Types.Core.Migration.Schedule where

import Beckn.Types.Core.Migration.Duration
import Data.Time
import EulerHS.Prelude

data Schedule = Schedule
  { _frequency :: Maybe Duration,
    _holidays :: Maybe [UTCTime],
    _times :: Maybe [UTCTime]
  }
  deriving (Generic, Show)

instance FromJSON Schedule where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Schedule where
  toJSON = genericToJSON stripAllLensPrefixOptions
