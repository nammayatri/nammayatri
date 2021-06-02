{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Migration.Vehicle where

import Beckn.Utils.JSON
import EulerHS.Prelude

data Vehicle = Vehicle
  { category :: Maybe Text,
    capacity :: Maybe Int,
    make :: Maybe Text,
    model :: Maybe Text,
    size :: Maybe Text,
    variant :: Maybe Text,
    color :: Maybe Text,
    energy_type :: Maybe Text,
    registration :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Vehicle where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Vehicle where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
