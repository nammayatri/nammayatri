{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Context where

import Data.Text
import Data.Time.LocalTime
import EulerHS.Prelude

data Context = Context
  { _domain :: Text, -- "MOBILITY", "FINAL-MILE-DELIVERY", "FOOD-AND-BEVERAGE", "HEALTHCARE"
    _country :: Maybe Text, -- Country code as per ISO 3166-1 and ISO 3166-2 format"
    _city :: Maybe Text, -- City code
    _action :: Text, -- "search", "select", "init", "confirm", "update", "status", "track", "cancel", "feedback", "support", "on_search", "on_select", "on_init", "on_confirm", "on_update", "on_status", "on_track", "on_cancel", "on_feedback", "on_support"
    _core_version :: Maybe Text,
    _domain_version :: Maybe Text,
    _bap_id :: Maybe Text,
    _bg_id :: Maybe Text,
    _bpp_id :: Maybe Text,
    _bap_nw_address :: Maybe Text,
    _bg_nw_address :: Maybe Text,
    _bpp_nw_address :: Maybe Text,
    _request_transaction_id :: Text,
    _timestamp :: LocalTime, -- ["format": "date-time"]
    _token :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Context where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Context where
  toJSON = genericToJSON stripAllLensPrefixOptions
