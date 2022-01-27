module Core.OnConfirm.Item where

import Beckn.Prelude
import Core.OnConfirm.Quantity

data Item = Item
  { route_code :: Text,
    start_stop :: Text,
    end_stop :: Text,
    start_time :: UTCTime,
    end_time :: UTCTime,
    quantity :: Quantity
  }
  deriving (Generic, Show, ToJSON, FromJSON)
