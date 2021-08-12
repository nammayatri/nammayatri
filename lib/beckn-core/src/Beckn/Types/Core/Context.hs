{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Context where

import Beckn.Types.App (BaseUrl)
import Beckn.Types.Core.Domain
import Beckn.Utils.Example
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text
import Data.Time
import EulerHS.Prelude
import Servant.Client (parseBaseUrl)

data Context = Context
  { domain :: Domain,
    country :: Maybe Text, -- Country code as per ISO 3166-1 and ISO 3166-2 format"
    city :: Maybe Text, -- City code
    action :: Text, -- "search", "select", "init", "confirm", "update", "status", "track", "cancel", "feedback", "support", "on_search", "on_select", "on_init", "on_confirm", "on_update", "on_status", "on_track", "on_cancel", "on_feedback", "on_support", "ack"
    core_version :: Maybe Text,
    domain_version :: Maybe Text,
    bap_uri :: Maybe BaseUrl, -- URL of the BAP
    bpp_uri :: Maybe BaseUrl, -- URL of the BPP
    transaction_id :: Text,
    message_id :: Text,
    timestamp :: UTCTime, -- ["format": "date-time"]
    ttl :: Maybe Text
  }
  deriving (Generic, FromJSON, Show, ToSchema)

instance Example Context where
  example =
    Context
      { domain = example,
        country = Just "IND",
        city = Just "BLR",
        action = "search",
        core_version = Just "0.8.2",
        domain_version = Just "0.8.2",
        bap_uri = parseBaseUrl "https://api.domain.com/",
        bpp_uri = parseBaseUrl "https://api.domain.com/",
        transaction_id = idExample,
        message_id = idExample,
        timestamp = example,
        ttl = Just "600"
      }

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}
