module Beckn.Types.FMD.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Payment
import Beckn.Types.FMD.Item (Item)
import Beckn.Types.FMD.Task (Task)
import Beckn.Utils.Common
import Data.Time.LocalTime
import EulerHS.Prelude

data Order = Order
  { _id :: Maybe Text,
    _state :: Maybe Text,
    _items :: [Item],
    _created_at :: LocalTime,
    _updated_at :: LocalTime,
    _tasks :: [Task],
    _billing :: Maybe Billing,
    _payment :: Maybe Payment
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Order where
  example =
    Order
      { _id = Just idExample,
        _state = Just "READY",
        _items = example,
        _created_at = example,
        _updated_at = example,
        _tasks = example,
        _billing = example,
        _payment = example
      }
