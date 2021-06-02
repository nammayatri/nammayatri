module Beckn.Types.Mobility.Route where

import Beckn.Types.Core.Scalar
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude

newtype Route = Route
  { edge :: RouteEdge
  }
  deriving (Generic, Show)

instance FromJSON Route where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Route where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Route where
  example =
    Route
      { edge = example
      }

data RouteEdge = RouteEdge
  { path :: String,
    duration :: Scalar,
    distance :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON RouteEdge where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON RouteEdge where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example RouteEdge where
  example =
    RouteEdge
      { path = "",
        duration = example,
        distance = example
      }
