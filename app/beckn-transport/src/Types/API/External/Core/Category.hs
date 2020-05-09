module Types.API.External.Core.Category where
  
import           Data.Text
import           EulerHS.Prelude

data Category =
  Category
    { _id :: Text
    , _subcategories :: [Category]
    }
      deriving (Generic, Show)

instance FromJSON Category where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Category where
  toJSON = genericToJSON stripAllLensPrefixOptions
