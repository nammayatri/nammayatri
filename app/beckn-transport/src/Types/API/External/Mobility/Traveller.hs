module Types.API.External.Mobility.Traveller where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Types.API.External.Core.Person
import           Types.API.External.Core.Rating
import           Types.API.External.Mobility.Stop

data Traveller =
  Traveller
    { _descriptor :: Person
    , _rating :: Rating
    , _origin :: Stop
    , _destination :: Stop
    }
      deriving (Generic, Show)

instance FromJSON Traveller where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Traveller where
  toJSON = genericToJSON stripLensPrefixOptions
