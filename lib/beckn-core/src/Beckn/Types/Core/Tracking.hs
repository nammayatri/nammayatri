{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Tracking where

import Beckn.Types.Core.Image
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
