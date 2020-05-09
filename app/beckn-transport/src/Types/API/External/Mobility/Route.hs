module Types.API.External.Mobility.Route where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Types.API.External.Mobility.Stop
import           Types.API.External.Core.Scalar

data Route =
  Route
    { _edge :: RouteEdge
    , _stops :: [Stop]
    }
      deriving (Generic, Show)

instance FromJSON Route where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Route where
  toJSON = genericToJSON stripLensPrefixOptions

data RouteEdge =
  RouteEdge
    { _endpoints :: Endpoint
    , _path :: String
    , _duration :: Scalar
    , _distance :: Scalar
    }
      deriving (Generic, Show)

instance FromJSON RouteEdge where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON RouteEdge where
  toJSON = genericToJSON stripLensPrefixOptions

data Endpoint =
  Endpoint
    { _start :: Stop
    , _stop :: Stop
    }
      deriving (Generic, Show)

instance FromJSON Endpoint where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Endpoint where
  toJSON = genericToJSON stripLensPrefixOptions