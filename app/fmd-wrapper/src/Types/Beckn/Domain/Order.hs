module Types.Beckn.Domain.Order where

import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude hiding (State)
import Types.Beckn.Billing
import Types.Beckn.Domain.Item (Item)
import Types.Beckn.Domain.Task (Task)
import Types.Beckn.Option (Option)
import Types.Beckn.Payment
import Types.Beckn.Quotation
import Types.Beckn.State

data Order = Order
  { _id :: Maybe Text,
    _state :: Maybe State,
    _items :: [Item],
    _created_at :: Maybe UTCTime,
    _updated_at :: Maybe UTCTime,
    _tasks :: [Task],
    _billing :: Maybe Billing,
    _payment :: Maybe Payment,
    _update_action :: Maybe Text,
    _quotation :: Maybe Quotation,
    -- Defines the type of order like a simple order, a return order, an internal order etc.
    -- The types of orders supported will be defined by the network ecosystem.
    _type :: Maybe Text,
    _prev_order_id :: Maybe Text,
    _return_reason_id :: Maybe Text,
    _cancellation_reasons :: Maybe [Option],
    _return_reasons :: Maybe [Option]
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
        _state = example,
        _items = example,
        _created_at = example,
        _updated_at = example,
        _tasks = example,
        _billing = example,
        _payment = example,
        _update_action = Nothing,
        _quotation = example,
        _type = Nothing,
        _prev_order_id = Nothing,
        _return_reason_id = Nothing,
        _cancellation_reasons = example,
        _return_reasons = example
      }
