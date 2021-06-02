module Beckn.Types.Core.Migration.Schedule where

import Beckn.Types.Core.Migration.Duration
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude

data Schedule = Schedule
  { frequency :: Maybe Duration,
    holidays :: Maybe [UTCTime],
    times :: Maybe [UTCTime]
  }
  deriving (Generic, Show)

instance FromJSON Schedule where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Schedule where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
