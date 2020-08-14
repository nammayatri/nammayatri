module Beckn.Types.FMD.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Payment
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.Item (Item)
import Beckn.Types.FMD.Task (Task)
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude

data Order = Order
  { _id :: Maybe Text,
    _state :: Maybe Text,
    _items :: [Item],
    _created_at :: UTCTime,
    _updated_at :: UTCTime,
    _tasks :: [Task],
    _billing :: Maybe Billing,
    _payment :: Maybe Payment,
    _update_action :: Maybe Text,
    _quotation :: Maybe Quotation,
    -- Defines the type of order like a simple order, a return order, an internal order etc.
    -- The types of orders supported will be defined by the network ecosystem.
    _type :: Maybe Text,
    _prev_order_id :: Maybe Text
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
        _payment = example,
        _update_action = Nothing,
        _quotation = Nothing,
        _type = Nothing,
        _prev_order_id = Nothing
      }
