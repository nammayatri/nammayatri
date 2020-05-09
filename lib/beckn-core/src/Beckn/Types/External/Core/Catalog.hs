module Beckn.Types.External.Core.Catalog where
  
import           Data.Text
import           EulerHS.Prelude
import           Beckn.Types.External.Core.Category
import           Beckn.Types.External.Core.Item

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
