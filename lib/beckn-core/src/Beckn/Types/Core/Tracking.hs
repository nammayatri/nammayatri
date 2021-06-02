{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Tracking where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude

data Tracking = Tracking
  { url :: Maybe Text,
    required_params :: Maybe Text,
    metadata :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Tracking where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Tracking where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Tracking where
  example =
    Tracking
      { url = Just "https://api.example.com/track",
        required_params = Just "",
        metadata = Just ""
      }
