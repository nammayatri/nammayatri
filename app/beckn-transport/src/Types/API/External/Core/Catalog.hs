module Types.API.External.Core.Catalog where
  
import           Data.Text
import           EulerHS.Prelude
import           Types.API.External.Core.Category
import           Types.API.External.Core.Item

data Catalog =
  Catalog
    { _category_tree :: Category
    , _items :: [Item]
    }
      deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Catalog where
  toJSON = genericToJSON stripLensPrefixOptions
