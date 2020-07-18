module Beckn.Types.FMD.Package where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Dimensions
import Beckn.Types.Core.Price
import Beckn.Types.Core.Scalar
import Beckn.Types.FMD.Item
import Beckn.Utils.Common
import EulerHS.Prelude

data Package = Package
  { _id :: Text,
    _parent_package_id :: Maybe Text,
    _descriptor :: Descriptor,
    _contents :: [Item],
    _price :: Price,
    _weight :: Scalar,
    _dimensions :: Dimensions
  }
  deriving (Generic, Show)

instance FromJSON Package where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Package where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Package where
  example =
    Package
      { _id = idExample,
        _parent_package_id = Nothing,
        _descriptor = example,
        _contents = example,
        _price = example,
        _weight = example,
        _dimensions = example
      }
