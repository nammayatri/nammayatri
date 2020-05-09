module Types.API.External.Core.Scalar where
  
import           Data.Text
import           EulerHS.Prelude
import           Types.API.External.Core.Contact
import           Types.API.External.Core.Api

data Scalar =
  Scalar
    { _value :: Double
    , _unit :: Text
    }
      deriving (Generic, Show)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Scalar where
  toJSON = genericToJSON stripAllLensPrefixOptions
