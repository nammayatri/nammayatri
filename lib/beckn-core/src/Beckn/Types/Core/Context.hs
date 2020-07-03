{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Context where

import Data.Text
import Data.Time.LocalTime
import EulerHS.Prelude

data Context = Context
  { domain :: Text, -- "MOBILITY", "FINAL-MILE-DELIVERY", "FOOD-AND-BEVERAGE"
    action :: Text, -- "search", "select", "confirm", "add", "remove", "complete", "cancel", "update", "on_search", "on_select", "on_confirm", "on_add", "on_remove", "on_cancel", "on_update", "register", "on_register", "lookup", "on_lookup", "tokens", "on_tokens"
    version :: Maybe Text,
    transaction_id :: Text,
    message_id :: Maybe Text,
    timestamp :: LocalTime, -- ["format": "date-time"]
    dummy :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
