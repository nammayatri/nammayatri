module Beckn.Types.Mobility.FareProduct where
  
import           Data.Text
import           EulerHS.Prelude
import           Beckn.Types.Core.Policy
import           Beckn.Types.Core.Item

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