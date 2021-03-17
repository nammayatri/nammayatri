module Beckn.Types.FMD.Package where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Dimensions
import Beckn.Types.Core.Price
import Beckn.Types.Core.Scalar
import Beckn.Types.FMD.Item
import Beckn.Utils.Example
import EulerHS.Prelude

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
