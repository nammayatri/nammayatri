{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Schedule where

import Beckn.Types.Core.Migration.Duration
import Beckn.Utils.JSON (deriveJSON)
import Data.Time
import EulerHS.Prelude

data Schedule = Schedule
  { _frequency :: Maybe Duration,
    _holidays :: Maybe [UTCTime],
    _times :: Maybe [UTCTime]
  }
  deriving (Generic, Show)

deriveJSON ''Schedule 'stripAllLensPrefixOptions
