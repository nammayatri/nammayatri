module Core.Spec.Confirm.Item where

import Beckn.Prelude

data Item = Item
  { route_code :: Text,
    start_stop :: Text,
    end_stop :: Text,
    start_time :: UTCTime,
    end_time :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
