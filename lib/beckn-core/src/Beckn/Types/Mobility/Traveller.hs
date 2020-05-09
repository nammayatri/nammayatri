module Beckn.Types.Mobility.Traveller where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Beckn.Types.Core.Person
import           Beckn.Types.Core.Rating
import           Beckn.Types.Mobility.Stop

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
