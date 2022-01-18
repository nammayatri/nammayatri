module Core.Time where

import Beckn.Prelude

data Time = Time
  { label :: Text,
    timestamp :: UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)
