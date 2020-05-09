module Beckn.Types.Mobility.Trip where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Beckn.Types.Mobility.Vehicle
import           Beckn.Types.Mobility.Driver
import           Beckn.Types.Mobility.Route
import           Beckn.Types.Mobility.Traveller
import           Beckn.Types.Mobility.Tracking
import           Beckn.Types.Core.Rating
import           Beckn.Types.Core.Price

data Trip =
  Trip
    { _id :: Text
    , _vehicle :: Vehicle
    , _driver :: TripDriver
    , _travellers :: [Traveller]
    , _tracking :: Tracking
    , _corridor_type :: Text --"FIXED","ON-DEMAND"
    , _state :: Text -- schema not available in github, so making it Text
    , _fare :: Price
    , _route :: Route
    }
      deriving (Generic, Show)

instance FromJSON Trip where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Trip where
  toJSON = genericToJSON stripLensPrefixOptions

data TripDriver =
  TripDriver
    { _persona :: Driver
    , _rating :: Rating
    }
      deriving (Generic, Show)

instance FromJSON TripDriver where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TripDriver where
  toJSON = genericToJSON stripLensPrefixOptions