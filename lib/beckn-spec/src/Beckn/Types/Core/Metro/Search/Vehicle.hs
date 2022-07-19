{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Metro.Search.Vehicle where

import Data.OpenApi (ToSchema)
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
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
