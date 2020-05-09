module Types.API.External.Core.Item where
  
import           Data.Text
import           EulerHS.Prelude
import           Types.API.External.Core.Policy
import           Types.API.External.Core.Image
import           Types.API.External.Core.Price

data Item =
  Item
    { _id :: Text
    , _name :: Text
    , _description :: Text
    , _image :: Image
    , _price :: Price
    , _primary :: Bool
    , _selected :: Bool
    , _quantity :: Integer
    , _policy :: Policy
    , _category_id :: Text
    , _tags :: [Text]
    }
      deriving (Generic, Show)

instance FromJSON Item where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Item where
  toJSON = genericToJSON stripAllLensPrefixOptions
