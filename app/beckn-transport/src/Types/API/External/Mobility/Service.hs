module Types.API.External.Mobility.Service where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Types.API.External.Core.Catalog
import           Types.API.External.Core.Item
import           Types.API.External.Core.Location
import           Types.API.External.Core.Offer
import           Types.API.External.Core.Policy
import           Types.API.External.Core.Provider
import           Types.API.External.Core.Scalar
import           Types.API.External.Mobility.FareProduct
import           Types.API.External.Mobility.Stop
import           Types.API.External.Mobility.Trip


data Service =
  Service
    { _id :: Text
    , _catalog :: Catalog
    , _matched_items :: [Text] -- [Item.id]
    , _selected_items :: [Text] -- [Item.id]
    , _fare_product :: FareProduct
    , _offers :: [Offer]
    , _provider :: Provider
    , _trip :: Trip
    , _policies :: [Policy]
    , _billing_address :: Location
    }
      deriving (Generic, Show)

instance FromJSON Service where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Service where
  toJSON = genericToJSON stripLensPrefixOptions

data ServiceEdge =
  ServiceEdge
    { _endpoints :: Endpoint
    , _path :: String
    , _duration :: Scalar
    , _distance :: Scalar
    }
      deriving (Generic, Show)

instance FromJSON ServiceEdge where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ServiceEdge where
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