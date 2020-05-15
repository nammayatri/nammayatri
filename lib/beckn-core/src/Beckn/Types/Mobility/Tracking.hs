{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Tracking where

import Beckn.Types.Core.Location
import Data.Generics.Labels
import Data.Text
import Data.Time
import EulerHS.Prelude

data Tracking = Tracking
  { method :: Text, -- "PULL", "PUSH"
    pull :: Maybe PullTrackingData
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data PullTrackingData = PullTrackingData
  { data_url :: Text,
    embed_url :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
