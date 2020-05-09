module Beckn.Types.Core.Provider where
  
import           Data.Text
import           EulerHS.Prelude
import           Beckn.Types.Core.Api
import           Beckn.Types.Core.Contact

data Provider =
  Provider
    { _id :: Text
    , _name :: Text
    , _website :: Text
    , _contact :: Contact
    , _api :: Api
    }
      deriving (Generic, Show)

instance FromJSON Provider where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Provider where
  toJSON = genericToJSON stripAllLensPrefixOptions
