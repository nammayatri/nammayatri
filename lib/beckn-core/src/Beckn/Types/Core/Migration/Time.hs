{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Time (Time (..), Range (..)) where

import Beckn.Types.Core.Migration.Duration (Duration)
import Beckn.Utils.JSON (deriveJSON)
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

deriveJSON ''Range 'stripAllLensPrefixOptions
deriveJSON ''Time 'stripAllLensPrefixOptions
