{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Context where

import Beckn.Types.Core.City
import Beckn.Types.Core.Country
import Data.Text
import Data.Time.LocalTime
import EulerHS.Prelude

data Context = Context
  { _domain :: Text, -- "MOBILITY", "FINAL-MILE-DELIVERY", "FOOD-AND-BEVERAGE", "HEALTHCARE"
    _country :: Maybe Country,
    _city :: Maybe City,
    _action :: Text, -- "search", "select", "init", "confirm", "update", "status", "track", "cancel", "feedback", "support", "on_search", "on_select", "on_init", "on_confirm", "on_update", "on_status", "on_track", "on_cancel", "on_feedback", "on_support"
    _core_version :: Maybe Text,
    _domain_version :: Maybe Text,
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
