module Types.Beckn.FmdOrder where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude hiding (State, id, state)
import Types.Beckn.Billing
import Types.Beckn.FmdItem (Item)
import Types.Beckn.Option (Option)
import Types.Beckn.Payment
import Types.Beckn.Quotation
import Types.Beckn.State
import Types.Beckn.Task (Task)

data Order = Order
  { id :: Maybe Text,
    state :: Maybe State,
    items :: [Item],
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime,
    tasks :: [Task],
    billing :: Maybe Billing,
    payment :: Maybe Payment,
    update_action :: Maybe Text,
    quotation :: Maybe Quotation,
    -- Defines the type of order like a simple order, a return order, an internal order etc.
    -- The types of orders supported will be defined by the network ecosystem.
    _type :: Maybe Text,
    prev_order_id :: Maybe Text,
    return_reason_id :: Maybe Text,
    cancellation_reasons :: Maybe [Option],
    return_reasons :: Maybe [Option]
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Order where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Order where
  example =
    Order
      { id = Just idExample,
        state = example,
        items = example,
        created_at = example,
        updated_at = example,
        tasks = example,
        billing = example,
        payment = example,
        update_action = Nothing,
        quotation = example,
        _type = Nothing,
        prev_order_id = Nothing,
        return_reason_id = Nothing,
        cancellation_reasons = example,
        return_reasons = example
      }
