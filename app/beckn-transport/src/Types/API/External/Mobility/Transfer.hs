module Types.API.External.Mobility.Transfer where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
-- import           Types.API.External.Mobility.Mode -- not availble in github
import           Types.API.External.Mobility.Route

data Transfer =
  Transfer
    { _mode :: Text
    , _route :: Route
    }
      deriving (Generic, Show)

instance FromJSON Transfer where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Transfer where
  toJSON = genericToJSON stripLensPrefixOptions
