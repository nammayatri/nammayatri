module Beckn.Types.External.Core.Scalar where
  
import           Data.Text
import           EulerHS.Prelude
import           Beckn.Types.External.Core.Contact
import           Beckn.Types.External.Core.Api

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
