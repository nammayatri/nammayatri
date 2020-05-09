module Types.API.External.Mobility.FareProduct where
  
import           Data.Text
import           EulerHS.Prelude
import           Types.API.External.Core.Policy
import           Types.API.External.Core.Item

data FareProduct =
  FareProduct
    { _id :: Text
    , _fare_media :: Text
    , _name :: Text
    , _fare_policy :: Policy
    , _applies_to_items :: [Item]
    }
      deriving (Generic, Show)

instance FromJSON FareProduct where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON FareProduct where
  toJSON = genericToJSON stripLensPrefixOptions