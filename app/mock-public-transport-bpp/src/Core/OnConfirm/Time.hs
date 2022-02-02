module Core.OnConfirm.Time where

import Data.Aeson
import Data.Time.Clock
import Relude

data Time = Time
  { label :: Text,
    timestamp :: UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)
