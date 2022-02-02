module Core.OnConfirm.Fulfillment where

import Core.OnConfirm.Time
import Data.Aeson
import Relude hiding (id)

data Fulfillment = Fulfillment
  { id :: Text,
    start :: FulfillmentLocationTime,
    end :: FulfillmentLocationTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FulfillmentLocationTime = FulfillmentLocationTime
  { location :: LocationId,
    time :: Time
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype FulfillmentId = FulfillmentId
  { id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype LocationId = LocationId
  { id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)
