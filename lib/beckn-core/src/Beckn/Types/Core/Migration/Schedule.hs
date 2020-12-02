{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Schedule where

import Beckn.Types.Core.Migration.Duration
import Data.Aeson.TH (deriveJSON)
import Data.Time
import EulerHS.Prelude

data Schedule = Schedule
  { _frequency :: Maybe Duration,
    _holidays :: [UTCTime],
    _times :: [UTCTime]
  }
  deriving (Generic, Show)

deriveJSON stripAllLensPrefixOptions ''Schedule
