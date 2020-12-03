module Beckn.Types.Core.Migration.Time (Time (..), Range (..)) where

import Beckn.Types.Core.Migration.Duration (Duration)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Time = Time
  { _label :: Maybe Text,
    _timestamp :: Maybe UTCTime,
    _duration :: Maybe Duration,
    _range :: Maybe Range,
    _days :: Text
  }
  deriving (Generic, Show)

data Range = Range
  { _start :: UTCTime,
    _end :: UTCTime
  }
  deriving (Generic, Show, Eq)

instance FromJSON Range where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Range where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Time where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Time where
  toJSON = genericToJSON stripAllLensPrefixOptions
