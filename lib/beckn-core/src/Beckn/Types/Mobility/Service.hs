{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Service where

import Beckn.Types.Core.Policy
import Beckn.Types.Core.Provider
import Beckn.Types.Mobility.Catalog
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude hiding (id)

data Service = Service
  { id :: Text,
    catalog :: Maybe Catalog,
    provider :: Maybe Provider,
    policies :: [Policy]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Service where
  example =
    Service
      { id = "123e4567-e89b-12d3-a456-426652340000",
        catalog = example,
        provider = example,
        policies = example
      }
