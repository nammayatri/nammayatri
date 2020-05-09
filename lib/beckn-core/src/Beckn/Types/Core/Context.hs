module Beckn.Types.Core.Context where

import Data.Text
import Data.Time.LocalTime
import EulerHS.Prelude

data Context = Context
  { _domain :: Text, -- "MOBILITY", "FINAL-MILE-DELIVERY", "FOOD-AND-BEVERAGE"
    _action :: Text, -- "search", "select", "confirm", "add", "remove", "complete", "cancel", "update", "on_search", "on_select", "on_confirm", "on_add", "on_remove", "on_cancel", "on_update", "register", "on_register", "lookup", "on_lookup", "tokens", "on_tokens"
    _version :: Maybe Text,
    _transaction_id :: Text,
    _message_id :: Maybe Text,
    _timestamp :: LocalTime, -- ["format": "date-time"]
    _dummy :: Text
  }
  deriving (Generic, Show)

instance FromJSON Context where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Context where
  toJSON = genericToJSON stripAllLensPrefixOptions
