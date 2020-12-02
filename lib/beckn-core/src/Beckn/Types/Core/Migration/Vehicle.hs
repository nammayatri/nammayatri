{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Vehicle where

import Beckn.Utils.JSON (deriveJSON)
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

deriveJSON ''Vehicle 'stripLensPrefixOptions
