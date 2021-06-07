module Types.Beckn.Package where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)
import Types.Beckn.Descriptor
import Types.Beckn.Dimensions
import Types.Beckn.FmdItem
import Types.Beckn.Price
import Types.Beckn.Scalar

data Package = Package
  { id :: Maybe Text,
    parent_package_id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    contents :: Maybe [Item],
    price :: Maybe Price,
    weight :: Maybe Scalar,
    dimensions :: Maybe Dimensions,
    package_category_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Package where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Package where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Package where
  example =
    Package
      { id = Just idExample,
        parent_package_id = Nothing,
        descriptor = example,
        contents = example,
        price = example,
        weight = example,
        dimensions = example,
        package_category_id = Just "1"
      }
