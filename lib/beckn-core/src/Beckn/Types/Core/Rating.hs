module Beckn.Types.Core.Rating where
  
import           Data.Text
import           EulerHS.Prelude
import           Beckn.Types.Core.Contact
import           Beckn.Types.Core.Api

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
