{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Service where

import Beckn.Types.Core.Policy
import Beckn.Types.Core.Provider
import Beckn.Types.Mobility.Catalog
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Service = Service
  { id :: Text,
    catalog :: Maybe Catalog,
    provider :: Maybe Provider,
    policies :: [Policy]
  }
  deriving (Generic, Show)

instance FromJSON Service where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Service where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Service where
  example =
    Service
      { id = "123e4567-e89b-12d3-a456-426652340000",
        catalog = example,
        provider = example,
        policies = example
      }
