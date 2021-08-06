module Beckn.Types.Mobility.Route where

import Beckn.Types.Core.Scalar
import Beckn.Utils.Example
import EulerHS.Prelude

newtype Route = Route
  { edge :: RouteEdge
  }
  deriving (Generic, FromJSON, ToJSON, Show)

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
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example RouteEdge where
  example =
    RouteEdge
      { path = "",
        duration = example,
        distance = example
      }
