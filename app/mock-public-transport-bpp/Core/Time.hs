module Core.Time where

import Beckn.Prelude

data Time = Time
  { label :: Text,
    timestamp :: UTCTime
    -- duration :: Maybe Duration,
    -- range :: Maybe Range,
    -- days :: Maybe Text
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)
