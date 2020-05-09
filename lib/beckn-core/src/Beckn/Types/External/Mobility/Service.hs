module Beckn.Types.External.Mobility.Service where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Beckn.Types.External.Core.Catalog
import           Beckn.Types.External.Core.Item
import           Beckn.Types.External.Core.Location
import           Beckn.Types.External.Core.Offer
import           Beckn.Types.External.Core.Policy
import           Beckn.Types.External.Core.Provider
import           Beckn.Types.External.Core.Scalar
import           Beckn.Types.External.Mobility.FareProduct
import           Beckn.Types.External.Mobility.Stop
import           Beckn.Types.External.Mobility.Trip


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