{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Context where

import Beckn.Types.Core.Domain
import Data.Aeson
import Data.Text
import Data.Time (UTCTime)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Context = Context
  { _domain :: Domain,
    _country :: Maybe Text, -- Country code as per ISO 3166-1 and ISO 3166-2 format"
    _city :: Maybe Text, -- City code
    _action :: Text, -- "search", "select", "init", "confirm", "update", "status", "track", "cancel", "feedback", "support", "on_search", "on_select", "on_init", "on_confirm", "on_update", "on_status", "on_track", "on_cancel", "on_feedback", "on_support", "ack"
    _core_version :: Maybe Text,
    _domain_version :: Maybe Text,
    _bap_uri :: Maybe BaseUrl, -- URL of the BAP
    _bpp_uri :: Maybe BaseUrl, -- URL of the BPP
    _transaction_id :: Text,
    _message_id :: Text,
    _timestamp :: UTCTime -- ["format": "date-time"]
  }
  deriving (Generic, Show)

instance FromJSON Context where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Context where
  toJSON = genericToJSON stripAllLensPrefixOptions
