{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Migration.Vehicle where

import EulerHS.Prelude

data Vehicle = Vehicle
  { _category :: Maybe Text,
    _capacity :: Maybe Int,
    _make :: Maybe Text,
    _model :: Maybe Text,
    _size :: Maybe Text,
    _variant :: Maybe Text,
    _color :: Maybe Text,
    _energy_type :: Maybe Text,
    _registration :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Vehicle where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Vehicle where
  toJSON = genericToJSON stripLensPrefixOptions
