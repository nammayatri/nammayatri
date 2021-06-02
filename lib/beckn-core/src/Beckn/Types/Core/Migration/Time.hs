module Beckn.Types.Core.Migration.Time (Time (..), Range (..)) where

import Beckn.Types.Core.Migration.Duration (Duration)
import Beckn.Utils.JSON
import Data.Time (UTCTime)
import EulerHS.Prelude

data Time = Time
  { label :: Maybe Text,
    timestamp :: Maybe UTCTime,
    duration :: Maybe Duration,
    range :: Maybe Range,
    days :: Text
  }
  deriving (Generic, Show)

data Range = Range
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, Show, Eq)

instance FromJSON Range where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Range where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Time where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Time where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
