module Beckn.Types.Mobility.Route where

import Beckn.Types.Core.Scalar
import Beckn.Types.Mobility.Stop
import Beckn.Utils.Common
import EulerHS.Prelude

newtype Route = Route
  { _edge :: RouteEdge
  }
  deriving (Generic, Show)

instance FromJSON Route where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Route where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Route where
  example =
    Route
      { _edge = example
      }

data RouteEdge = RouteEdge
  { _path :: String,
    _duration :: Scalar,
    _distance :: Scalar,
    _stops :: [Stop]
  }
  deriving (Generic, Show)

instance FromJSON RouteEdge where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON RouteEdge where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example RouteEdge where
  example =
    RouteEdge
      { _path = "",
        _duration = example,
        _distance = example,
        _stops = example
      }
