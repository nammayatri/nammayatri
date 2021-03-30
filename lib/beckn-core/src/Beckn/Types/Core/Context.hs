{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Context where

import Beckn.Types.Core.Domain
import Beckn.Utils.Example
import Data.Aeson
import Data.Text
import Data.Time
import EulerHS.Prelude
import Servant.Client (BaseUrl, parseBaseUrl)

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
    _timestamp :: UTCTime, -- ["format": "date-time"]
    _ttl :: Maybe Text
  }
  deriving (Generic, Show)

instance Example Context where
  example =
    Context
      { _domain = example,
        _country = Just "IND",
        _city = Just "BLR",
        _action = "search",
        _core_version = Just "0.8.2",
        _domain_version = Just "0.8.2",
        _bap_uri = parseBaseUrl "https://api.domain.com/",
        _bpp_uri = parseBaseUrl "https://api.domain.com/",
        _transaction_id = idExample,
        _message_id = idExample,
        _timestamp = example,
        _ttl = Just "600"
      }

instance FromJSON Context where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Context where
  toJSON = genericToJSON $ stripAllLensPrefixOptions {omitNothingFields = True}
