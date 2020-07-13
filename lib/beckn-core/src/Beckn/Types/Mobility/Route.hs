module Beckn.Types.Mobility.Route where

import Beckn.Types.Core.Scalar
import Beckn.Types.Mobility.Stop
import Beckn.Utils.Common
import Data.Text
import Data.Time
import EulerHS.Prelude

data Route = Route
  { _edge :: RouteEdge,
    _stops :: [Stop]
  }
  deriving (Generic, Show)

instance FromJSON Route where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Route where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Route where
  example =
    Route
      { _edge = example,
        _stops = example
      }

data RouteEdge = RouteEdge
  { _endpoints :: Endpoint,
    _path :: String,
    _duration :: Scalar,
    _distance :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON RouteEdge where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON RouteEdge where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example RouteEdge where
  example =
    RouteEdge
      { _endpoints = example,
        _path = "",
        _duration = example,
        _distance = example
      }

data Endpoint = Endpoint
  { _start :: Stop,
    _stop :: Stop
  }
  deriving (Generic, Show)

instance FromJSON Endpoint where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Endpoint where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Endpoint where
  example =
    Endpoint
      { _start = example,
        _stop = example
      }
