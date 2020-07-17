{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Tracking where

import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Tracking = Tracking
  { method :: Text, -- "PULL", "PUSH"
    pull :: Maybe PullTrackingData
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Tracking where
  example =
    Tracking
      { method = "PULL",
        pull = example
      }

data PullTrackingData = PullTrackingData
  { data_url :: Text,
    embed_url :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example PullTrackingData where
  example =
    PullTrackingData
      { data_url = "http://localhost:8080/",
        embed_url = "http://localhost:8080/"
      }
