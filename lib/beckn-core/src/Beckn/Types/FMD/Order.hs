module Beckn.Types.FMD.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Dimensions
import Beckn.Types.Core.Fulfillment
import Beckn.Types.Core.Price
import Beckn.Types.Core.Scalar
import Beckn.Types.FMD.Item
import Beckn.Types.FMD.Task (Task)
import Beckn.Utils.Common
import Data.Time.LocalTime
import EulerHS.Prelude

data Order = Order
  { _id :: Text,
    _state :: Text,
    _billing :: Maybe Billing,
    _fulfillment :: Maybe Fulfillment,
    _created_at :: LocalTime,
    _updated_at :: LocalTime,
    _tasks :: [Task],
    _packages :: [OrderPackage]
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Order where
  example =
    Order
      { _id = idExample,
        _state = "READY",
        _billing = Nothing,
        _fulfillment = Nothing,
        _created_at = example,
        _updated_at = example,
        _tasks = example,
        _packages = example
      }

data OrderPackage = OrderPackage
  { _id :: Text,
    _parent_package_id :: Maybe Text,
    _descriptor :: Descriptor,
    _contents :: [Item],
    _price :: Price,
    _weight :: Scalar,
    _dimensions :: Dimensions,
    _state :: Text,
    _task_id :: Text,
    _created_at :: LocalTime,
    _updated_at :: LocalTime
  }
  deriving (Generic, Show)

instance FromJSON OrderPackage where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON OrderPackage where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example OrderPackage where
  example =
    OrderPackage
      { _id = idExample,
        _parent_package_id = Nothing,
        _descriptor = example,
        _contents = example,
        _price = example,
        _weight = example,
        _dimensions = example,
        _state = "READY",
        _task_id = idExample,
        _created_at = example,
        _updated_at = example
      }
