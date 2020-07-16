{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Service where

import Beckn.Types.Core.Policy
import Beckn.Types.Core.Provider
import Beckn.Types.Mobility.Catalog
import Beckn.Types.Mobility.FareProduct
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Trip
import Beckn.Utils.Common
import Data.Generics.Labels
import Data.Text
import Data.Time
import EulerHS.Prelude

data Service = Service
  { _id :: Text,
    _catalog :: Maybe Catalog,
    _provider :: Maybe Provider,
    _policies :: [Policy]
  }
  deriving (Generic, Show)

instance FromJSON Service where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Service where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Service where
  example =
    Service
      { _id = "123e4567-e89b-12d3-a456-426652340000",
        _catalog = example,
        _provider = example,
        _policies = example
      }
