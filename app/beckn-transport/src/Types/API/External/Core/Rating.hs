module Types.API.External.Core.Rating where
  
import           Data.Text
import           EulerHS.Prelude
import           Types.API.External.Core.Contact
import           Types.API.External.Core.Api

data Rating =
  Rating
    { _value :: Text
    , _scale :: [Text]
    }
      deriving (Generic, Show)

instance FromJSON Rating where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Rating where
  toJSON = genericToJSON stripAllLensPrefixOptions
