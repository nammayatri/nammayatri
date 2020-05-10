{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Service where

import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Item
import Beckn.Types.Core.Location
import Beckn.Types.Core.Offer
import Beckn.Types.Core.Policy
import Beckn.Types.Core.Provider
import Beckn.Types.Core.Scalar
import Beckn.Types.Mobility.FareProduct
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Trip
import Data.Generics.Labels
import Data.Text
import Data.Time
import EulerHS.Prelude


data Service = Service
  { _id :: Text,
    _catalog :: Catalog,
    _matched_items :: [Text], -- [Item.id]
    _selected_items :: [Text], -- [Item.id]
    _fare_product :: FareProduct,
    _offers :: [Offer],
    _provider :: Provider,
    _trip :: Trip,
    _policies :: [Policy],
    _billing_address :: Location
  }
  deriving (Generic, Show)

instance FromJSON Service where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Service where
  toJSON = genericToJSON stripLensPrefixOptions

data ServiceEdge = ServiceEdge
  { _endpoints :: Endpoint,
    _path :: String,
    _duration :: Scalar,
    _distance :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON ServiceEdge where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ServiceEdge where
  toJSON = genericToJSON stripLensPrefixOptions

data Endpoint = Endpoint
  { _start :: Stop,
    _stop :: Stop
  }
  deriving (Generic, Show)

instance FromJSON Endpoint where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Endpoint where
  toJSON = genericToJSON stripLensPrefixOptions
