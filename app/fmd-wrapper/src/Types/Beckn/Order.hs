module Types.Beckn.Order where

import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (State, id, state)
import Types.Beckn.Billing
import Types.Beckn.Fulfillment (Fulfillment)
import Types.Beckn.Payment

newtype OrderObject = OrderObject
  { order :: Order
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Order = Order
  { state :: Text,
    items :: [OrderItem],
    billing :: Billing,
    fulfillment :: Fulfillment,
    payment :: Payment,
    updated_at :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype OrderItem = OrderItem
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
