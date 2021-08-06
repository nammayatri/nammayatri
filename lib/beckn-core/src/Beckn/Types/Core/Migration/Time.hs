module Beckn.Types.Core.Migration.Time (Time (..), Range (..)) where

import Beckn.Types.Core.Migration.Duration (Duration)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Time = Time
  { label :: Maybe Text,
    timestamp :: Maybe UTCTime,
    duration :: Maybe Duration,
    range :: Maybe Range,
    days :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Range = Range
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
