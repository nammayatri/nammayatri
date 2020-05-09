module Types.API.External.Core.Tag where
  
import           Data.Text
import           EulerHS.Prelude
import           Types.API.External.Core.Contact
import           Types.API.External.Core.Api

data Tag =
  Tag
    { _label :: Text
    , _value :: Text
    }
      deriving (Generic, Show)

instance FromJSON Tag where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tag where
  toJSON = genericToJSON stripAllLensPrefixOptions
