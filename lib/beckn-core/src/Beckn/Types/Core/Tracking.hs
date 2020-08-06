{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Tracking where

import Beckn.Utils.Common
import EulerHS.Prelude

data Tracking = Tracking
  { _url :: Maybe Text,
    _required_params :: Maybe Text,
    _metadata :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Tracking where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tracking where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Tracking where
  example =
    Tracking
      { _url = Just "https://api.example.com/track",
        _required_params = Just "",
        _metadata = Just ""
      }
