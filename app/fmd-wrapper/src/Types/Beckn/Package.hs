module Types.Beckn.Package where

import Beckn.Utils.Example
import EulerHS.Prelude
import Types.Beckn.Descriptor
import Types.Beckn.Dimensions
import Types.Beckn.FmdItem
import Types.Beckn.Price
import Types.Beckn.Scalar

data Package = Package
  { _id :: Maybe Text,
    _parent_package_id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _contents :: Maybe [Item],
    _price :: Maybe Price,
    _weight :: Maybe Scalar,
    _dimensions :: Maybe Dimensions,
    _package_category_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Package where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Package where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Package where
  example =
    Package
      { _id = Just idExample,
        _parent_package_id = Nothing,
        _descriptor = example,
        _contents = example,
        _price = example,
        _weight = example,
        _dimensions = example,
        _package_category_id = Just "1"
      }
