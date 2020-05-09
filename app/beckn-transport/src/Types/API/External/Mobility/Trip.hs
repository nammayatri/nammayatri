module Types.API.External.Mobility.Trip where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Types.API.External.Mobility.Vehicle
import           Types.API.External.Mobility.Driver
import           Types.API.External.Mobility.Route
import           Types.API.External.Mobility.Traveller
import           Types.API.External.Mobility.Tracking
import           Types.API.External.Core.Rating
import           Types.API.External.Core.Price

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