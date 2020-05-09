module Types.API.External.Core.Api where
  
import           Data.Text
import           EulerHS.Prelude
import           Data.Time.LocalTime

data Api =
  Api
    { _url :: Text
    , _exp :: LocalTime
    } deriving (Generic, Show)

instance FromJSON Api where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Api where
  toJSON = genericToJSON stripAllLensPrefixOptions
