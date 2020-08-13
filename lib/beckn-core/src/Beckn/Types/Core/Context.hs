{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Context where

import Data.Text
import Data.Time (UTCTime)
import EulerHS.Prelude

data Context = Context
  { _domain :: Text, -- "MOBILITY", "FINAL-MILE-DELIVERY", "FOOD-AND-BEVERAGE", "HEALTHCARE"
    _country :: Maybe Text, -- Country code as per ISO 3166-1 and ISO 3166-2 format"
    _city :: Maybe Text, -- City code
    _action :: Text, -- "search", "select", "init", "confirm", "update", "status", "track", "cancel", "feedback", "support", "on_search", "on_select", "on_init", "on_confirm", "on_update", "on_status", "on_track", "on_cancel", "on_feedback", "on_support"
    _core_version :: Maybe Text,
    _domain_version :: Maybe Text,
    _ac_id :: Maybe Text, -- Caller ID. This is the ID of the BAP, BPP or BG who is initiating the transaction
    _transaction_id :: Text,
    _message_id :: Text,
    _timestamp :: UTCTime -- ["format": "date-time"]
  }
  deriving (Generic, Show)

instance FromJSON Context where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Context where
  toJSON = genericToJSON stripAllLensPrefixOptions
